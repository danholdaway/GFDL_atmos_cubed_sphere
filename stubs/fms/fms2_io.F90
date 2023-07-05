module fms2_io_mod
    use platform_mod, only: r4_kind, r8_kind
    use mpp_domains_mod
    use fms_io_mod, only: get_mosaic_tile_grid
    implicit none

    public :: open_file, close_file, get_mosaic_tile_grid

    public :: read_data

type, private :: bc_information
  integer, dimension(:), allocatable :: indices !< Indices for the halo region for the variable
                                                !! (starting x, ending x, starting y, ending y)
  integer, dimension(:), allocatable :: global_size !< Size of the variable for each dimension
  integer, dimension(:), allocatable :: pelist !< List of pelist that have the data for the variable
  logical :: is_root_pe !< Flag indicating if this is the root_pe from the pelist
  integer :: x_halo !< Number of halos in x
  integer :: y_halo !< Number of halos in y
  integer :: jshift !< Shift in the x axis (from center)
  integer :: ishift !< Shift in the y axis (from center)
  real(kind=r4_kind), dimension(:,:), allocatable :: globaldata2d_r4 !< 2d data pointer.
  real(kind=r4_kind), dimension(:,:,:), allocatable :: globaldata3d_r4 !< 3d data pointer.
  real(kind=r8_kind), dimension(:,:), allocatable :: globaldata2d_r8 !< 2d data pointer.
  real(kind=r8_kind), dimension(:,:,:), allocatable :: globaldata3d_r8 !< 3d data pointer.
  character(len=32) :: chksum !< The variable's checksum
  logical :: data_on_file_root !< Flag indicating if the file root is part of the pelist that
                               !!contains data
endtype bc_information

!> @brief Restart variable.
!> @ingroup netcdf_io_mod
type, private :: RestartVariable_t
  character(len=256) :: varname !< Variable name.
  class(*), pointer :: data0d => null() !< Scalar data pointer.
  class(*), dimension(:), pointer :: data1d => null() !< 1d data pointer.
  class(*), dimension(:,:), pointer :: data2d => null() !< 2d data pointer.
  class(*), dimension(:,:,:), pointer :: data3d => null() !< 3d data pointer.
  class(*), dimension(:,:,:,:), pointer :: data4d => null() !< 4d data pointer.
  class(*), dimension(:,:,:,:,:), pointer :: data5d => null() !< 5d data pointer.
  logical :: was_read !< Flag to support legacy "query_initialized" feature, which
                      !! keeps track if a file was read.
  logical :: is_bc_variable !< Flag indicating if variable is a bc_variable
  type(bc_information) :: bc_info !< information about the boundary condition variable
endtype RestartVariable_t

type, private :: CompressedDimension_t
  character(len=256) :: dimname !< Dimension name.
  integer, dimension(:), allocatable :: npes_corner !< Array of starting
                                                    !! indices for each rank.
  integer, dimension(:), allocatable :: npes_nelems !< Number of elements
                                                    !! associated with each
                                                    !! rank.
  integer :: nelems !< Total size of the dimension.
endtype CompressedDimension_t

type, private :: dimension_information
  integer, dimension(5) :: xlen !> The size of each unique x dimension
  integer, dimension(5) :: ylen !> The size of each unique y dimension
  integer, dimension(5) :: zlen !> The size of each unique z dimension
  integer, dimension(3) :: cur_dim_len !> Number of unique:
                                       !! cur_dim_len(1) : x dimensions
                                       !! cur_dim_len(2) : y dimensions
                                       !! cur_dim_len(3) : z dimensions
endtype dimension_information

type, public :: FmsNetcdfFile_t
  character(len=256) :: path !< File path.
  logical :: is_readonly !< Flag telling if the file is readonly.
  integer :: ncid !< Netcdf file id.
  character(len=256) :: nc_format !< Netcdf file format.
  logical :: is_netcdf4 !< Flag indicating if the netcdf file type is netcdf4
  integer, dimension(:), allocatable :: pelist !< List of ranks who will
                                               !! communicate.
  integer :: io_root !< I/O root rank of the pelist.
  logical :: is_root !< Flag telling if the current rank is the
                     !! I/O root.
  logical :: is_restart !< Flag telling if the this file is a restart
                        !! file (that has internal pointers to data).
  logical :: mode_is_append !! true if file is open in "append" mode
  logical, allocatable :: is_open !< Allocated and set to true if opened.
  type(RestartVariable_t), dimension(:), allocatable :: restart_vars !< Array of registered
                                                                     !! restart variables.
  integer :: num_restart_vars !< Number of registered restart variables.
  type(CompressedDimension_t), dimension(:), allocatable :: compressed_dims !< "Compressed" dimension.
  integer :: num_compressed_dims !< Number of compressed dimensions.
  logical :: is_diskless !< Flag telling whether this is a diskless file.
  character (len=20) :: time_name
  type(dimension_information) :: bc_dimensions !<information about the current dimensions for regional
                                               !! restart variables

endtype FmsNetcdfFile_t

type, private :: DomainDimension_t
  character(len=256) :: varname !< Variable name.
  integer :: pos !< Domain position.
endtype DomainDimension_t

type, extends(FmsNetcdfFile_t), public :: FmsNetcdfDomainFile_t
  type(domain2d) :: domain !< Two-dimensional domain.
  type(DomainDimension_t), dimension(:), allocatable :: xdims !< Dimensions associated
                                                              !! with the "x" axis
                                                              !! of a 2d domain.
  integer :: nx !< Number of "x" dimensions.
  type(DomainDimension_t), dimension(:), allocatable :: ydims !< Dimensions associated
                                                              !! with the "y" axis
                                                              !! of a 2d domain.
  integer :: ny !< Number of "y" dimensions.
  character(len=256) :: non_mangled_path !< Non-domain-mangled file path.
  logical :: adjust_indices !< Flag telling if indices need to be adjusted
                            !! for domain-decomposed read.
endtype FmsNetcdfDomainFile_t

interface read_data
  module procedure read_data_netcdfdomain_1d
  module procedure read_data_netcdfdomain_2d
  module procedure read_data_netcdf_char
  module procedure read_data_netcdf_1d
  module procedure read_data_netcdf_2d
end interface

interface open_file
   module procedure open_file_netcfd
   module procedure open_file_string
end interface

interface close_file
   module procedure close_file_netcfd
   module procedure close_file_int
end interface 
    
contains

integer function open_file_string(file, form, action, access, threading, recl, dist)
   character(len=*), intent(in) :: file
   character(len=*), intent(in), optional :: form, action, access, threading
   integer         , intent(in), optional :: recl
   logical         , intent(in), optional :: dist  ! Distributed open?
end function open_file_string

integer function open_file_netcfd(file, form, action, access, threading, recl, dist)
   type(FmsNetcdfFile_t), intent(in) :: file
   character(len=*), intent(in), optional :: form, action, access, threading
   integer         , intent(in), optional :: recl
   logical         , intent(in), optional :: dist  ! Distributed open?
end function open_file_netcfd

subroutine close_file_int(unit, status, dist)
   integer,          intent(in)           :: unit
   character(len=*), intent(in), optional :: status
   logical,          intent(in), optional :: dist
end subroutine close_file_int

subroutine close_file_netcfd(unit, status, dist)
   type(FmsNetcdfFile_t), intent(in) :: unit
   character(len=*), intent(in), optional :: status
   logical,          intent(in), optional :: dist
end subroutine close_file_netcfd

subroutine get_variable_attribute(file, string, arg, arg2)
  type(FmsNetcdfFile_t), intent(in) :: file
  character(len=*), intent(in) :: string, arg
  character(len=*), intent(inout) :: arg2
end subroutine get_variable_attribute

subroutine get_global_attribute(file, string, arg)
  type(FmsNetcdfFile_t), intent(in) :: file
  character(len=*), intent(in) :: string
  character(len=*), intent(inout) :: arg
end subroutine get_global_attribute

subroutine read_data_netcdfdomain_1d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfDomainFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  real, intent(inout) :: dst(:)
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdfdomain_1d

subroutine read_data_netcdfdomain_2d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfDomainFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  real, intent(inout) :: dst(:,:)
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdfdomain_2d

subroutine read_data_netcdf_char(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  character(len=*), intent(inout) :: dst
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdf_char

subroutine read_data_netcdf_1d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  real, intent(inout) :: dst(:)
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdf_1d

subroutine read_data_netcdf_2d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  real, intent(inout) :: dst(:,:)
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdf_2d

logical function variable_exists(fileobj, variable_name, broadcast)
  class(FmsNetcdfFile_t), intent(in) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  logical, intent(in), optional :: broadcast !< Flag controlling whether or
end function variable_exists

logical function file_exists(path) 
  character(len=*), intent(in) :: path
end function file_exists

subroutine ascii_read(ascii_filename, ascii_var, num_lines, max_length)
  character(len=*), intent(in) :: ascii_filename 
  character(len=:), dimension(:), allocatable, intent(out) :: ascii_var 
  integer, optional, intent(out) :: num_lines 
  integer, optional, intent(out) :: max_length
end subroutine ascii_read

subroutine set_filename_appendix(z)
    character(len=6), intent (in) :: z
 end subroutine set_filename_appendix
    
end module fms2_io_mod