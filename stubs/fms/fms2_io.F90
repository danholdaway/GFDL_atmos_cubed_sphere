module fms2_io_mod
    use platform_mod, only: r4_kind, r8_kind
    use mpp_domains_mod
    use fms_io_mod, only: get_mosaic_tile_grid, set_filename_appendix
    implicit none

    public :: open_file, close_file, get_mosaic_tile_grid, set_filename_appendix

    public :: read_data

    integer :: unlimited

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
  ! Removed below comment dude to Tapenade "syntax error"
  ! class(*), pointer :: data0d => null() !< Scalar data pointer.
  ! class(*), dimension(:), pointer :: data1d => null() !< 1d data pointer.
  ! class(*), dimension(:,:), pointer :: data2d => null() !< 2d data pointer.
  ! class(*), dimension(:,:,:), pointer :: data3d => null() !< 3d data pointer.
  ! class(*), dimension(:,:,:,:), pointer :: data4d => null() !< 4d data pointer.
  ! class(*), dimension(:,:,:,:,:), pointer :: data5d => null() !< 5d data pointer.
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

type, public :: FmsNetcdfDomainFile_t
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

  ! TODO: manually extend FmsNetcdfDomainFile_t with FmsNetcdfFile_t to avoid Taepnade error due to `extends(FmsNetcdfFile_t)`
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
endtype FmsNetcdfDomainFile_t

interface read_data
  module procedure read_data_netcdfdomain_r_1d
  module procedure read_data_netcdfdomain_r_2d
  module procedure read_data_netcdfdomain_r_3d
  module procedure read_data_netcdf_char
  module procedure read_data_netcdf_i_0d
  module procedure read_data_netcdf_r_1d
  module procedure read_data_netcdf_r_2d
  module procedure read_data_netcdf_r_3d
end interface

interface open_file
   module procedure open_file_netcdf
   module procedure open_file_string
   module procedure open_file_netcdfdomain
end interface

interface close_file
   module procedure close_file_netcdf
   module procedure close_file_int
   module procedure close_file_netcdfdomain
end interface 

interface register_restart_field
    module procedure register_restart_field_netcdf_2d
    module procedure register_restart_field_netcdf_3d_other
    module procedure register_restart_field_netcdf_4d_other
    module procedure register_restart_field_netcdfdomain_2d
    module procedure register_restart_field_netcdfdomain_3d
    module procedure register_restart_field_netcdfdomain_3d_other
end interface

interface register_axis 
    module procedure register_axis_netcdf
    module procedure register_axis_netcdfdomain_1
    module procedure register_axis_netcdfdomain_2
end interface

interface read_restart 
    module procedure read_restart_netcdf
    module procedure read_restart_netcdfdomain
end interface

interface write_restart 
    module procedure write_restart_netcdf
    module procedure write_restart_netcdfdomain
end interface

interface register_field
    module procedure register_field_netcdf
    module procedure register_field_netcdfdomain_1d
    module procedure register_field_netcdfdomain_3d
end interface

interface write_data
    module procedure write_data_netcdf_i_0d
    module procedure write_data_netcdf_i_1d
    module procedure write_data_netcdfdomain_i_0d
    module procedure write_data_netcdfdomain_i_1d
    module procedure write_data_netcdfdomain_r_1d
    module procedure write_data_netcdfdomain_r_2d
    module procedure write_data_netcdfdomain_r_3d
end interface

interface register_variable_attribute
    module procedure register_variable_attribute_netcdf
    module procedure register_variable_attribute_netcdfdomain
end interface

interface register_global_attribute
      module procedure register_global_attribute_i
      module procedure register_global_attribute_str
      module procedure register_global_attribute_r
end interface

interface variable_exists
      module procedure variable_exists_netcdfdomain
      module procedure variable_exists_netcdf
end interface 
    
contains

integer function get_variable_num_dimensions(file, name)
  type(FmsNetcdfDomainFile_t), intent(in) :: file
  character(len=*), intent(in) :: name
end function get_variable_num_dimensions

subroutine get_variable_dimension_names(file, name, dim_names)
  type(FmsNetcdfDomainFile_t), intent(in) :: file
  character(len=*), intent(in) :: name
  character(len=8), intent(out) :: dim_names(:)
end subroutine get_variable_dimension_names

subroutine get_variable_size(file, name, size)
  type(FmsNetcdfFile_t), intent(in) :: file
  character(len=*), intent(in) :: name
  integer, intent(out) :: size(:)
end subroutine get_variable_size

subroutine get_dimension_size(file, name, val)
  type(FmsNetcdfFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name
  integer, intent(out) :: val
end subroutine get_dimension_size

logical function global_att_exists(file, name)
type(FmsNetcdfFile_t), intent(inout) :: file
character(len=*), intent(in) :: name
end function global_att_exists

subroutine register_global_attribute_i(file, name, val)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name
  integer, intent(in) :: val
end subroutine register_global_attribute_i

subroutine register_global_attribute_r(file, name, val)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name
  real, intent(in) :: val
end subroutine register_global_attribute_r

subroutine register_global_attribute_str(file, name, val)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name, val
end subroutine register_global_attribute_str

subroutine register_variable_attribute_netcdf(file, str1, str2, str3, str_len)
  type(FmsNetcdfFile_t), intent(inout) :: file
  character(len=*), intent(in) :: str1, str2, str3
  integer, optional, intent(in) :: str_len
end subroutine register_variable_attribute_netcdf

subroutine register_variable_attribute_netcdfdomain(file, str1, str2, str3, str_len)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: str1, str2, str3
  integer, optional, intent(in) :: str_len
end subroutine register_variable_attribute_netcdfdomain

subroutine get_global_io_domain_indices(file, axisname, is, ie, buffer)
  type(FmsNetcdfDomainFile_t), intent(inout) ::  file
  integer, intent(in) :: ie, is
  integer, allocatable, intent(inout) :: buffer(:)
  character(len=7) :: axisname
end subroutine get_global_io_domain_indices

subroutine write_data_netcdf_i_0d(file, name, buffer)
  type(FmsNetcdfFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name
  integer, intent(in) :: buffer
end subroutine write_data_netcdf_i_0d

subroutine write_data_netcdf_i_1d(file, name, buffer)
  type(FmsNetcdfFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name
  integer, intent(in) :: buffer(:)
end subroutine write_data_netcdf_i_1d

subroutine write_data_netcdfdomain_i_0d(file, name, buffer)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name
  integer, intent(in) :: buffer
end subroutine write_data_netcdfdomain_i_0d

subroutine write_data_netcdfdomain_i_1d(file, name, buffer)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name
  integer, intent(in) :: buffer(:)
end subroutine write_data_netcdfdomain_i_1d

subroutine write_data_netcdfdomain_r_1d(file, name, buffer)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name
  real, intent(in) :: buffer(:)
end subroutine write_data_netcdfdomain_r_1d

subroutine write_data_netcdfdomain_r_2d(file, name, buffer)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name
  real, intent(in) :: buffer(:,:)
end subroutine write_data_netcdfdomain_r_2d

subroutine write_data_netcdfdomain_r_3d(file, name, buffer)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: name
  real, intent(in) :: buffer(:,:,:)
end subroutine write_data_netcdfdomain_r_3d

subroutine register_field_netcdf(file, name, field_type, dim_names)
  type(FmsNetcdfFile_t), intent(in) :: file
  character(len=*), intent(in) :: name, field_type
  character(len=*), intent(in)  :: dim_names(:)
end subroutine register_field_netcdf

subroutine register_field_netcdfdomain_1d(file, name, field_type, dim_names)
  type(FmsNetcdfDomainFile_t), intent(in) :: file
  character(len=*), intent(in) :: name, field_type
  character(len=*), intent(in)  :: dim_names(:)
end subroutine register_field_netcdfdomain_1d

subroutine register_field_netcdfdomain_3d(file, name, field_type, dim_names)
  type(FmsNetcdfDomainFile_t), intent(in) :: file
  character(len=*), intent(in) :: name, field_type
  character(len=*), intent(in)  :: dim_names(:,:,:)
end subroutine register_field_netcdfdomain_3d

subroutine write_restart_bc(file)
  type(FmsNetcdfFile_t), intent(inout) :: file
end subroutine write_restart_bc

subroutine write_restart_netcdf(file)
  type(FmsNetcdfFile_t), intent(inout) :: file
end subroutine write_restart_netcdf

subroutine write_restart_netcdfdomain(file)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
end subroutine write_restart_netcdfdomain

subroutine read_restart_bc(file, ignore_checksum)
  type(FmsNetcdfFile_t), intent(inout) :: file
  logical, optional, intent(in) :: ignore_checksum
end subroutine read_restart_bc

subroutine read_restart_netcdf(file, ignore_checksum)
  type(FmsNetcdfFile_t), intent(inout) :: file
  logical, optional, intent(in) :: ignore_checksum
end subroutine read_restart_netcdf

subroutine read_restart_netcdfdomain(file, ignore_checksum)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  logical, optional, intent(in) :: ignore_checksum
end subroutine read_restart_netcdfdomain

subroutine register_axis_netcdf(file, str1, int, domain_position)
  type(FmsNetcdfFile_t), intent(inout) :: file
  character(len=*), intent(in) :: str1
  integer, intent(in) :: int
  integer, optional, intent(in) :: domain_position
end subroutine register_axis_netcdf

subroutine register_axis_netcdfdomain_1(file, str1, domain_position)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: str1
  integer, optional, intent(in) :: domain_position
end subroutine register_axis_netcdfdomain_1

subroutine register_axis_netcdfdomain_2(file, str1, str2, domain_position)
  type(FmsNetcdfDomainFile_t), intent(inout) :: file
  character(len=*), intent(in) :: str1
  character(len=*), intent(in) :: str2
  integer, optional, intent(in) :: domain_position
end subroutine register_axis_netcdfdomain_2

subroutine register_restart_field_netcdf_2d(file, str, ak, dim_names)
    type(FmsNetcdfFile_t), intent(in) :: file
    character(len=*), intent(in) :: str
    real, intent(in) :: ak(:)
    character(len=8), dimension(2), intent(in)  :: dim_names
end subroutine register_restart_field_netcdf_2d

subroutine register_restart_field_netcdf_3d_other(file, str, ak, indices, global_size, pelist, is_root_pe, jshift, x_halo, y_halo, ishift, is_optional)
  type(FmsNetcdfFile_t), intent(in) :: file
  character(len=*), intent(in) :: str
  real, dimension(:,:), intent(in) :: ak
  integer, intent(in) :: indices(:), global_size(:), pelist(:)
  logical, intent(in) :: is_root_pe
  integer, optional, intent(in) :: jshift, x_halo, y_halo, ishift
  logical, optional, intent(in) :: is_optional
end subroutine register_restart_field_netcdf_3d_other

subroutine register_restart_field_netcdf_4d_other(file, str, ak, indices, global_size, pelist, is_root_pe, jshift, x_halo, y_halo, ishift, is_optional)
  type(FmsNetcdfFile_t), intent(in) :: file
  character(len=*), intent(in) :: str
  real, intent(in) :: ak(:,:,:)
  integer, intent(in) :: indices(:), global_size(:), pelist(:)
  logical, intent(in) :: is_root_pe
  integer, optional, intent(in) :: jshift, x_halo, y_halo, ishift
  logical, optional, intent(in) :: is_optional
end subroutine register_restart_field_netcdf_4d_other

subroutine register_restart_field_netcdfdomain_2d(file, str, ak, dim_names)
  type(FmsNetcdfDomainFile_t), intent(in) :: file
  character(len=*), intent(in) :: str
  real, intent(in) :: ak(:,:)
  character(len=*), dimension(:), intent(in)  :: dim_names
end subroutine register_restart_field_netcdfdomain_2d

subroutine register_restart_field_netcdfdomain_3d(file, str, ak, dim_names, is_optional)
  type(FmsNetcdfDomainFile_t), intent(in) :: file
  character(len=*), intent(in) :: str
  real, intent(in) :: ak(:,:,:)
  character(len=8), dimension(:), intent(in)  :: dim_names
  logical, optional, intent(in) :: is_optional
end subroutine register_restart_field_netcdfdomain_3d

subroutine register_restart_field_netcdfdomain_3d_other(file, str, ak, indices, global_size, pelist, is_root_pe, jshift, x_halo, y_halo, ishift)
  type(FmsNetcdfDomainFile_t), intent(in) :: file
  character(len=*), intent(in) :: str
  real, intent(in) :: ak(:,:,:)
  integer, intent(in) :: indices(:), global_size(:), pelist(:)
  logical, intent(in) :: is_root_pe
  integer, optional, intent(in) :: jshift, x_halo, y_halo, ishift
end subroutine register_restart_field_netcdfdomain_3d_other

logical function open_file_string(file, form, action, access, threading, recl, dist)
   character(len=*), intent(in) :: file
   character(len=*), intent(in), optional :: form, action, access, threading
   integer         , intent(in), optional :: recl
   logical         , intent(in), optional :: dist  ! Distributed open?
end function open_file_string

logical function open_file_netcdf(file, form, action, access, threading, recl, dist, is_restart, pelist)
   type(FmsNetcdfFile_t), intent(in) :: file
   character(len=*), intent(in), optional :: form, action, access, threading
   integer         , intent(in), optional :: recl
   logical         , intent(in), optional :: dist  ! Distributed open?
   logical         , intent(in), optional :: is_restart
   integer, allocatable, dimension(:), intent(in), optional :: pelist 
end function open_file_netcdf

logical function open_file_netcdfdomain(file, name, action, fv_domain, is_restart, pelist, dont_add_res_to_filename)
   type(FmsNetcdfDomainFile_t), intent(in) :: file
   character(len=*), intent(in), optional :: name, action
   type(domain2d),   intent(in), optional :: fv_domain
   logical         , intent(in), optional :: is_restart, dont_add_res_to_filename
   integer, allocatable, dimension(:), intent(in), optional :: pelist 
end function open_file_netcdfdomain

subroutine close_file_int(unit, status, dist)
   integer,          intent(in)           :: unit
   character(len=*), intent(in), optional :: status
   logical,          intent(in), optional :: dist
end subroutine close_file_int

subroutine close_file_netcdf(unit, status, dist)
   type(FmsNetcdfFile_t), intent(in) :: unit
   character(len=*), intent(in), optional :: status
   logical,          intent(in), optional :: dist
end subroutine close_file_netcdf

subroutine close_file_netcdfdomain(unit, status, dist)
  type(FmsNetcdfDomainFile_t), intent(in) :: unit
  character(len=*), intent(in), optional :: status
  logical,          intent(in), optional :: dist
end subroutine close_file_netcdfdomain

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

subroutine read_data_netcdfdomain_r_1d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfDomainFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  real, intent(inout) :: dst(:)
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdfdomain_r_1d

subroutine read_data_netcdfdomain_r_2d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfDomainFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  real, intent(inout) :: dst(:,:)
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdfdomain_r_2d

subroutine read_data_netcdfdomain_r_3d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfDomainFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  real, intent(inout) :: dst(:,:,:)
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdfdomain_r_3d

subroutine read_data_netcdf_char(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  character(len=*), intent(inout) :: dst
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdf_char

subroutine read_data_netcdf_i_0d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  integer, intent(inout) :: dst
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdf_i_0d

subroutine read_data_netcdf_r_1d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  real, intent(inout) :: dst(:)
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdf_r_1d

subroutine read_data_netcdf_r_2d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  real, intent(inout) :: dst(:,:)
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdf_r_2d

subroutine read_data_netcdf_r_3d(src, string, dst, corner, edge_lengths)
  type(FmsNetcdfFile_t), intent(in) :: src
  character(len=*), intent(in) :: string
  real, intent(inout) :: dst(:,:,:)
  integer, optional, intent(in) :: corner(:), edge_lengths(:)
end subroutine read_data_netcdf_r_3d

logical function variable_exists_netcdfdomain(fileobj, variable_name, broadcast)
  class(FmsNetcdfDomainFile_t), intent(in) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  logical, intent(in), optional :: broadcast !< Flag controlling whether or
end function variable_exists_netcdfdomain

logical function variable_exists_netcdf(fileobj, variable_name, broadcast)
  class(FmsNetcdfFile_t), intent(in) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  logical, intent(in), optional :: broadcast !< Flag controlling whether or
end function variable_exists_netcdf

logical function file_exists(path) 
  character(len=*), intent(in) :: path
end function file_exists

subroutine ascii_read(ascii_filename, ascii_var, num_lines, max_length)
  character(len=*), intent(in) :: ascii_filename 
  character(len=:), dimension(:), allocatable, intent(out) :: ascii_var 
  integer, optional, intent(out) :: num_lines 
  integer, optional, intent(out) :: max_length
end subroutine ascii_read
    
end module fms2_io_mod