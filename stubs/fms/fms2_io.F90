module fms2_io_mod
    use platform_mod, only: r4_kind, r8_kind
    use mpp_domains_mod
    implicit none

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
    
contains

subroutine set_filename_appendix(z)
    character(len=6), intent (in) :: z
 end subroutine set_filename_appendix
    
end module fms2_io_mod