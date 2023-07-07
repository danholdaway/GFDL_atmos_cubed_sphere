module mpp_domains_mod 

  use platform_mod, only: i8_kind

  use mpp_parameter_mod, only: GLOBAL_DATA_DOMAIN, BGRID_NE, FOLD_NORTH_EDGE, CGRID_NE, MPP_DOMAIN_TIME, &
                               CYCLIC_GLOBAL_DOMAIN, NUPDATE, EUPDATE, XUPDATE, YUPDATE, SCALAR_PAIR, &
                               NORTH_EAST, SOUTH_EAST, SOUTH_WEST, NORTH_WEST, AGRID, DGRID_NE, &
                               BITWISE_EFP_SUM
implicit none
private

public DGRID_NE, CGRID_NE, &
       mpp_get_boundary, mpp_update_domains, &
       nest_domain_type, &
       domain1d, domain2d, domainUG, &
       mpp_get_data_domain, mpp_get_compute_domain, mpp_get_global_domain, &
       CENTER, CORNER, NORTH, EAST, SOUTH, WEST, &
       mpp_get_pelist, &
       mpp_global_field, &
       mpp_get_C2F_index, mpp_get_F2C_index, &
       mpp_update_nest_fine, &
       mpp_update_nest_coarse, &
       mpp_group_update_type, &
       mpp_global_sum, BITWISE_EXACT_SUM, GLOBAL_DATA_DOMAIN, BGRID_NE, &
       FOLD_NORTH_EDGE, MPP_DOMAIN_TIME, CYCLIC_GLOBAL_DOMAIN, &
       NUPDATE, EUPDATE, XUPDATE, YUPDATE, SCALAR_PAIR, mpp_get_ntile_count, &
       mpp_global_max, mpp_global_min, mpp_domains_init, mpp_domains_exit, &
       mpp_broadcast_domain, mpp_check_field, mpp_define_layout, &
       mpp_define_mosaic_pelist, mpp_get_neighbor_pe, mpp_define_io_domain, &
       NORTH_EAST, SOUTH_EAST, SOUTH_WEST, NORTH_WEST, mpp_start_group_update, &
       mpp_complete_group_update, domaincommunicator2D, mpp_define_mosaic, &
       mpp_group_update_initialized, mpp_create_group_update, mpp_reset_group_update_field, &
       mpp_get_io_domain_layout, mpp_get_layout, mpp_copy_domain, mpp_do_group_update, &
       mpp_get_domain_shift, AGRID, BITWISE_EFP_SUM, mpp_get_tile_id, &
       mpp_get_compute_domains, mpp_get_domain_components, mpp_domains_set_stack_size, &
       mpp_define_nest_domains


  !!!!!!!!!FV_ARRAYS!!!!!!!!!!!!
  integer, parameter :: FVPRC = 8
  integer, parameter :: r_grid = 8
  !!!!!!!!!FV_ARRAYS!!!!!!!!!!!!


  integer :: mpp_check_field ! dummy
  integer :: mpp_domains_exit ! dummy
  integer :: mpp_get_domain_shift ! dummy
  integer :: mpp_get_domain_components ! dummy
  integer :: mpp_domains_set_stack_size ! dummy
  integer :: CENTER, CORNER, NORTH, EAST, SOUTH, WEST
  integer, parameter :: BITWISE_EXACT_SUM = 1

  integer, parameter :: MAXOVERLAP = 100
  integer, parameter :: NAME_LENGTH = 64
  integer, parameter :: MAX_DOMAIN_FIELDS=100
  integer, parameter :: MAX_REQUEST = 100
  integer, parameter :: LONG_KIND = 4

  type domain_axis_spec
   private
   integer :: dummy1
  end type domain_axis_spec

  type domain1D
   private
   integer :: dummy1
  end type domain1D

  type domain1D_spec
   private
   integer :: dummy1
  end type domain1D_spec

  type overlap_type
   private
   integer :: dummy1
  end type overlap_type

  type overlapSpec
   private
   integer :: dummy1
  end type overlapSpec

  type domain2D_spec
   private
   integer :: dummy1
  end type domain2D_spec

  type tile_type
   private
   integer :: dummy1
  end type tile_type

  type index_type
   private
   integer :: dummy1
  end type index_type

  type nestSpec
   private
   integer :: dummy1
  end type nestSpec

!  type nest_domain_type
!   private
!   integer :: dummy1
!  end type nest_domain_type

  type mpp_group_update_type
   private
   integer :: dummy1
  end type mpp_group_update_type

!  type domain_axis_spec        !type used to specify index limits along an axis of a domain
!     private
!     integer :: begin, end, size, max_size      !start, end of domain axis, size, max size in set
!     logical :: is_global       !TRUE if domain axis extent covers global domain
!  end type domain_axis_spec
!
!  type domain1D
!     private
!     type(domain_axis_spec) :: compute, data, global, memory
!     logical :: cyclic
!     type(domain1D), pointer :: list(:) =>NULL()
!     integer :: pe               !PE to which this domain is assigned
!     integer :: pos              !position of this PE within link list, i.e domain%list(pos)%pe = pe
!     integer :: goffset, loffset !needed for global sum
!  end type domain1D
!
!  type domain1D_spec
!     private
!     type(domain_axis_spec) :: compute
!     integer                :: pos
!  end type domain1D_spec
!
!  type overlap_type
!     private
!     integer                  :: count = 0                 ! number of ovrelapping
!     integer                  :: pe
!     integer                  :: start_pos                 ! start position in the buffer
!     integer                  :: totsize                   ! all message size
!     integer ,        pointer :: msgsize(:)      => NULL() ! overlapping msgsize to be sent or received
!     integer,         pointer :: tileMe(:)       => NULL() ! my tile id for this overlap
!     integer,         pointer :: tileNbr(:)      => NULL() ! neighbor tile id for this overlap
!     integer,         pointer :: is(:)           => NULL() ! starting i-index 
!     integer,         pointer :: ie(:)           => NULL() ! ending   i-index 
!     integer,         pointer :: js(:)           => NULL() ! starting j-index 
!     integer,         pointer :: je(:)           => NULL() ! ending   j-index 
!     integer,         pointer :: dir(:)          => NULL() ! direction ( value 1,2,3,4 = E,S,W,N)
!     integer,         pointer :: rotation(:)     => NULL() ! rotation angle.
!     integer,         pointer :: index(:)        => NULL() ! for refinement
!     logical,         pointer :: from_contact(:) => NULL() ! indicate if the overlap is computed from define_contact_overlap
!  end type overlap_type
!
!  type overlapSpec
!     private
!     integer                     :: whalo, ehalo, shalo, nhalo ! halo size
!     integer                     :: xbegin, xend, ybegin, yend
!     integer                     :: nsend, nrecv
!     integer                     :: sendsize, recvsize
!     type(overlap_type), pointer :: send(:) => NULL()
!     type(overlap_type), pointer :: recv(:) => NULL()
!     type(overlapSpec),  pointer :: next
!  end type overlapSpec
!
!  type domain2D_spec
!     private
!     type(domain1D_spec), pointer :: x(:)       => NULL() ! x-direction domain decomposition
!     type(domain1D_spec), pointer :: y(:)       => NULL() ! x-direction domain decomposition
!     integer,        pointer :: tile_id(:) => NULL() ! tile id of each tile
!     integer                 :: pe                   ! PE to which this domain is assigned
!     integer                 :: pos                  ! position of this PE within link list
!     integer                 :: tile_root_pe         ! root pe of tile.
!  end type domain2D_spec
!
!  type tile_type
!     integer :: xbegin, xend, ybegin, yend
!  end type tile_type

  type domain2D
     private
     character(len=NAME_LENGTH)  :: name='unnamed'          ! name of the domain, default is "unspecified"
     integer(LONG_KIND)          :: id 
     integer                     :: pe                      ! PE to which this domain is assigned
     integer                     :: fold          
     integer                     :: pos                     ! position of this PE within link list
     logical                     :: symmetry                ! indicate the domain is symmetric or non-symmetric.
     integer                     :: whalo, ehalo            ! halo size in x-direction
     integer                     :: shalo, nhalo            ! halo size in y-direction
     integer                     :: ntiles                  ! number of tiles within mosaic
     integer                     :: max_ntile_pe            ! maximum value in the pelist of number of tiles on each pe.
     integer                     :: ncontacts               ! number of contact region within mosaic.
     logical                     :: rotated_ninety          ! indicate if any contact rotate NINETY or MINUS_NINETY
     logical                     :: initialized=.FALSE.     ! indicate if the overlapping is computed or not.
     integer                     :: tile_root_pe            ! root pe of current tile.
     integer                     :: io_layout(2)            ! io_layout, will be set through mpp_define_io_domain
                                                            ! default = domain layout
     integer,            pointer :: pearray(:,:)  => NULL() ! pe of each layout position 
     integer,            pointer :: tile_id(:)    => NULL() ! tile id of each tile
     type(domain1D),     pointer :: x(:)          => NULL() ! x-direction domain decomposition
     type(domain1D),     pointer :: y(:)          => NULL() ! y-direction domain decomposition
     type(domain2D_spec),pointer :: list(:)       => NULL() ! domain decomposition on pe list
     type(tile_type),    pointer :: tileList(:)   => NULL() ! store tile information
     type(overlapSpec),  pointer :: check_C       => NULL() ! send and recv information for boundary consistency check of C-cell
     type(overlapSpec),  pointer :: check_E       => NULL() ! send and recv information for boundary consistency check of E-cell
     type(overlapSpec),  pointer :: check_N       => NULL() ! send and recv information for boundary consistency check of N-cell
     type(overlapSpec),  pointer :: bound_C       => NULL() ! send information for getting boundary value for symmetry domain.
     type(overlapSpec),  pointer :: bound_E       => NULL() ! send information for getting boundary value for symmetry domain.
     type(overlapSpec),  pointer :: bound_N       => NULL() ! send information for getting boundary value for symmetry domain.
     type(overlapSpec),  pointer :: update_T      => NULL() ! send and recv information for halo update of T-cell.
     type(overlapSpec),  pointer :: update_E      => NULL() ! send and recv information for halo update of E-cell.
     type(overlapSpec),  pointer :: update_C      => NULL() ! send and recv information for halo update of C-cell.
     type(overlapSpec),  pointer :: update_N      => NULL() ! send and recv information for halo update of N-cell.
     type(domain2d),     pointer :: io_domain     => NULL() ! domain for IO, will be set through calling mpp_set_io_domain ( this will be changed).
  end type domain2D   

type :: domainUG
  private
  ! type(unstruct_axis_spec) :: compute, global !< axis specifications
  ! type(unstruct_domain_spec), pointer :: list(:)=>NULL() !<
  ! type(domainUG), pointer :: io_domain=>NULL() !<
  ! type(unstruct_pass_type) :: SG2UG
  ! type(unstruct_pass_type) :: UG2SG
  ! integer, pointer :: grid_index(:) => NULL() !< index of grid on current pe
  ! type(domain2d), pointer :: SG_domain => NULL()
  integer :: pe
  integer :: pos
  integer :: ntiles
  integer :: tile_id
  integer :: tile_root_pe
  integer :: tile_npes
  integer :: npes_io_group
  integer(kind=4) :: io_layout
end type domainUG

  type nest_domain_type
     private
     integer                    :: tile_fine, tile_coarse
     integer                    :: istart_fine, iend_fine, jstart_fine, jend_fine
     integer                    :: istart_coarse, iend_coarse, jstart_coarse, jend_coarse
     integer                    :: x_refine, y_refine
     logical                    :: is_fine_pe, is_coarse_pe
     integer,           pointer :: pelist_fine(:) => NULL()
     integer,           pointer :: pelist_coarse(:) => NULL()
     character(len=NAME_LENGTH) :: name
     type(nestSpec), pointer :: C2F_T => NULL()
     type(nestSpec), pointer :: C2F_C => NULL()
     type(nestSpec), pointer :: C2F_E => NULL()
     type(nestSpec), pointer :: C2F_N => NULL()
     type(nestSpec), pointer :: F2C_T => NULL()
     type(nestSpec), pointer :: F2C_C => NULL()
     type(nestSpec), pointer :: F2C_E => NULL()
     type(nestSpec), pointer :: F2C_N => NULL()
     type(domain2d), pointer :: domain_fine   => NULL()
     type(domain2d), pointer :: domain_coarse => NULL()
  end type nest_domain_type

!  type index_type
!     integer :: is_me, ie_me, js_me, je_me
!     integer :: is_you, ie_you, js_you, je_you
!  end type index_type
!
!  type nestSpec
!     private
!     integer                     :: xbegin, xend, ybegin, yend
!     type(index_type)            :: west, east, south, north, center
!     integer                     :: nsend, nrecv
!     integer                     :: extra_halo
!     type(overlap_type), pointer :: send(:) => NULL()
!     type(overlap_type), pointer :: recv(:) => NULL()
!     type(nestSpec),     pointer :: next
!  end type nestSpec
!
!  type nest_domain_type
!     private
!     integer                    :: tile_fine, tile_coarse
!     integer                    :: istart_fine, iend_fine, jstart_fine, jend_fine
!     integer                    :: istart_coarse, iend_coarse, jstart_coarse, jend_coarse
!     integer                    :: x_refine, y_refine
!     logical                    :: is_fine_pe, is_coarse_pe
!     integer,           pointer :: pelist_fine(:) => NULL()
!     integer,           pointer :: pelist_coarse(:) => NULL()
!     character(len=NAME_LENGTH) :: name
!     type(nestSpec), pointer :: C2F_T => NULL()
!     type(nestSpec), pointer :: C2F_C => NULL()
!     type(nestSpec), pointer :: C2F_E => NULL()
!     type(nestSpec), pointer :: C2F_N => NULL()
!     type(nestSpec), pointer :: F2C_T => NULL()
!     type(nestSpec), pointer :: F2C_C => NULL()
!     type(nestSpec), pointer :: F2C_E => NULL()
!     type(nestSpec), pointer :: F2C_N => NULL()
!     type(domain2d), pointer :: domain_fine   => NULL()
!     type(domain2d), pointer :: domain_coarse => NULL()
!  end type nest_domain_type
!
!  type mpp_group_update_type
!     private
!     logical            :: initialized = .FALSE.
!     logical            :: k_loop_inside = .TRUE.
!     integer            :: nscalar = 0
!     integer            :: nvector = 0
!     integer            :: flags_s=0, flags_v=0
!     integer            :: whalo_s=0, ehalo_s=0, shalo_s=0, nhalo_s=0
!     integer            :: isize_s=0, jsize_s=0, ksize_s=1
!     integer            :: whalo_v=0, ehalo_v=0, shalo_v=0, nhalo_v=0
!     integer            :: isize_x=0, jsize_x=0, ksize_v=1
!     integer            :: isize_y=0, jsize_y=0
!     integer            :: position=0, gridtype=0
!     logical            :: recv_s(8), recv_v(8)
!     integer            :: is_s=0, ie_s=0, js_s=0, je_s=0
!     integer            :: is_x=0, ie_x=0, js_x=0, je_x=0
!     integer            :: is_y=0, ie_y=0, js_y=0, je_y=0
!     integer            :: nrecv=0, nsend=0
!     integer            :: npack=0, nunpack=0
!     integer            :: reset_index_s = 0
!     integer            :: reset_index_v = 0
!     integer            :: tot_msgsize = 0
!     integer            :: from_pe(MAXOVERLAP)
!     integer            :: to_pe(MAXOVERLAP)
!     integer            :: recv_size(MAXOVERLAP)
!     integer            :: send_size(MAXOVERLAP)
!     integer            :: buffer_pos_recv(MAXOVERLAP)
!     integer            :: buffer_pos_send(MAXOVERLAP)
!     integer            :: pack_type(MAXOVERLAP)
!     integer            :: pack_buffer_pos(MAXOVERLAP)
!     integer            :: pack_rotation(MAXOVERLAP)
!     integer            :: pack_size(MAXOVERLAP)
!     integer            :: pack_is(MAXOVERLAP)
!     integer            :: pack_ie(MAXOVERLAP)
!     integer            :: pack_js(MAXOVERLAP)
!     integer            :: pack_je(MAXOVERLAP)
!     integer            :: unpack_type(MAXOVERLAP)
!     integer            :: unpack_buffer_pos(MAXOVERLAP)
!     integer            :: unpack_rotation(MAXOVERLAP)
!     integer            :: unpack_size(MAXOVERLAP)
!     integer            :: unpack_is(MAXOVERLAP)
!     integer            :: unpack_ie(MAXOVERLAP)
!     integer            :: unpack_js(MAXOVERLAP)
!     integer            :: unpack_je(MAXOVERLAP)
!     integer(LONG_KIND) :: addrs_s(MAX_DOMAIN_FIELDS)
!     integer(LONG_KIND) :: addrs_x(MAX_DOMAIN_FIELDS)
!     integer(LONG_KIND) :: addrs_y(MAX_DOMAIN_FIELDS)
!     integer            :: buffer_start_pos = -1
!     integer            :: request_send(MAX_REQUEST)
!     integer            :: request_recv(MAX_REQUEST)
!     integer            :: type_recv(MAX_REQUEST)
!  end type mpp_group_update_type

  type :: domaincommunicator2D
  private
  logical            :: initialized=.false.
  integer(i8_kind) :: id=-9999
  integer(i8_kind) :: l_addr  =-9999
  integer(i8_kind) :: l_addrx =-9999
  integer(i8_kind) :: l_addry =-9999
  type(domain2D), pointer :: domain     =>NULL()
  type(domain2D), pointer :: domain_in  =>NULL()
  type(domain2D), pointer :: domain_out =>NULL()
  type(overlapSpec), pointer :: send(:,:,:,:) => NULL()
  type(overlapSpec), pointer :: recv(:,:,:,:) => NULL()
  integer, dimension(:,:),       allocatable :: sendis
  integer, dimension(:,:),       allocatable :: sendie
  integer, dimension(:,:),       allocatable :: sendjs
  integer, dimension(:,:),       allocatable :: sendje
  integer, dimension(:,:),       allocatable :: recvis
  integer, dimension(:,:),       allocatable :: recvie
  integer, dimension(:,:),       allocatable :: recvjs
  integer, dimension(:,:),       allocatable :: recvje
  logical, dimension(:),         allocatable :: S_do_buf
  logical, dimension(:),         allocatable :: R_do_buf
  integer, dimension(:),         allocatable :: cto_pe
  integer, dimension(:),         allocatable :: cfrom_pe
  integer, dimension(:),         allocatable :: S_msize
  integer, dimension(:),         allocatable :: R_msize
  integer :: Slist_size=0, Rlist_size=0
  integer :: isize=0, jsize=0, ke=0
  integer :: isize_in=0, jsize_in=0
  integer :: isize_out=0, jsize_out=0
  integer :: isize_max=0, jsize_max=0
  integer :: gf_ioff=0, gf_joff=0
! Remote data
  integer, dimension(:)  , allocatable :: isizeR
  integer, dimension(:)  , allocatable :: jsizeR
  integer, dimension(:,:), allocatable :: sendisR
  integer, dimension(:,:), allocatable :: sendjsR
  integer(i8_kind), dimension(:), allocatable :: rem_addr
  integer(i8_kind), dimension(:), allocatable :: rem_addrx
  integer(i8_kind), dimension(:), allocatable :: rem_addry
  integer(i8_kind), dimension(:,:), allocatable :: rem_addrl
  integer(i8_kind), dimension(:,:), allocatable :: rem_addrlx
  integer(i8_kind), dimension(:,:), allocatable :: rem_addrly
  integer                             :: position !< data location. T, E, C, or N.
end type DomainCommunicator2D


  interface mpp_update_nest_fine
     module procedure mpp_update_nest_fine_r4_2d
     module procedure mpp_update_nest_fine_r4_3d
     module procedure mpp_update_nest_fine_r4_4d
     module procedure mpp_update_nest_fine_r4_2d_v
     module procedure mpp_update_nest_fine_r4_3d_v
     module procedure mpp_update_nest_fine_r8_2d
     module procedure mpp_update_nest_fine_r8_3d
     module procedure mpp_update_nest_fine_r8_4d
     module procedure mpp_update_nest_fine_r8_2d_v
     module procedure mpp_update_nest_fine_r8_3d_v
  end interface


  interface mpp_update_nest_coarse
     module procedure mpp_update_nest_coarse_r4_2d
     module procedure mpp_update_nest_coarse_r4_3d
     module procedure mpp_update_nest_coarse_r4_4d
     module procedure mpp_update_nest_coarse_r4_3d_v
     module procedure mpp_update_nest_coarse_r8_2d
     module procedure mpp_update_nest_coarse_r8_3d
     module procedure mpp_update_nest_coarse_r8_4d
     module procedure mpp_update_nest_coarse_r8_3d_v
  end interface

  interface mpp_global_field
     module procedure mpp_global_field2d_r4_2d
     module procedure mpp_global_field2d_r4_3d
     module procedure mpp_global_field2d_r4_4d
     module procedure mpp_global_field2d_r4_5d
     module procedure mpp_global_field2d_r8_2d
     module procedure mpp_global_field2d_r8_3d
     module procedure mpp_global_field2d_r8_4d
     module procedure mpp_global_field2d_r8_5d
  end interface

  interface mpp_update_domains
     module procedure mpp_update_domain2D_r4_2d
     module procedure mpp_update_domain2D_r4_3d
     module procedure mpp_update_domain2D_r4_4d
     module procedure mpp_update_domain2D_r4_5d
     module procedure mpp_update_domain2D_r4_2dv
     module procedure mpp_update_domain2D_r4_3dv
     module procedure mpp_update_domain2D_r4_4dv
     module procedure mpp_update_domain2D_r4_5dv
     module procedure mpp_update_domain2D_r8_2d
     module procedure mpp_update_domain2D_r8_3d
     module procedure mpp_update_domain2D_r8_4d
     module procedure mpp_update_domain2D_r8_5d
     module procedure mpp_update_domain2D_r8_2dv
     module procedure mpp_update_domain2D_r8_3dv
     module procedure mpp_update_domain2D_r8_4dv
     module procedure mpp_update_domain2D_r8_5dv
  end interface

interface mpp_global_sum
     module procedure mpp_global_sum_2d
     module procedure mpp_global_sum_3d
     module procedure mpp_global_sum_4d
     module procedure mpp_global_sum_5d
end interface

interface mpp_global_max
  module procedure mpp_global_max_r4
  module procedure mpp_global_max_r8
end interface

interface mpp_global_min
module procedure mpp_global_min_r4
module procedure mpp_global_min_r8
end interface

interface mpp_create_group_update
module procedure mpp_create_group_update_r4_2d
module procedure mpp_create_group_update_r4_3d
module procedure mpp_create_group_update_r4_4d
module procedure mpp_create_group_update_r4_2dv
module procedure mpp_create_group_update_r4_3dv
module procedure mpp_create_group_update_r8_2d
module procedure mpp_create_group_update_r8_3d
module procedure mpp_create_group_update_r8_4d
module procedure mpp_create_group_update_r8_2dv
module procedure mpp_create_group_update_r8_3dv
end interface mpp_create_group_update

interface mpp_reset_group_update_field
module procedure mpp_reset_group_update_field_r4_2d
module procedure mpp_reset_group_update_field_r4_3d
module procedure mpp_reset_group_update_field_r4_4d
module procedure mpp_reset_group_update_field_r4_2dv
module procedure mpp_reset_group_update_field_r4_3dv
module procedure mpp_reset_group_update_field_r8_2d
module procedure mpp_reset_group_update_field_r8_3d
module procedure mpp_reset_group_update_field_r8_4d
module procedure mpp_reset_group_update_field_r8_2dv
module procedure mpp_reset_group_update_field_r8_3dv
end interface mpp_reset_group_update_field

interface mpp_get_boundary
module procedure mpp_get_boundary_r4_2d
module procedure mpp_get_boundary_r4_3d
module procedure mpp_get_boundary_r8_2d
module procedure mpp_get_boundary_r8_3d
end interface

 contains

 subroutine mpp_define_nest_domains(nest_domain, domain, num_nest, nest_level, tile_fine, tile_coarse, &
  istart_coarse, icount_coarse, jstart_coarse, jcount_coarse, npes_nest_tile, &
  x_refine, y_refine, extra_halo, name)
type(nest_domain_type),     intent(inout) :: nest_domain 
type(domain2D), target,     intent(in   ) :: domain 
integer,                    intent(in   ) :: num_nest 
integer,                    intent(in   ) :: nest_level(:) 
integer,                    intent(in   ) :: tile_fine(:), tile_coarse(:) 
integer, intent(in ) :: istart_coarse(:), icount_coarse(:), jstart_coarse(:), jcount_coarse(:)
integer,                    intent(in   ) :: npes_nest_tile(:)
integer,                    intent(in   ) :: x_refine(:), y_refine(:) 
integer,          optional, intent(in   ) :: extra_halo 
character(len=*), optional, intent(in   ) :: name 
 end subroutine mpp_define_nest_domains

 subroutine mpp_get_global_domain( domain, xbegin, xend, ybegin, yend, xsize, xmax_size, ysize, ymax_size, &
  tile_count, position )
type(domain2D),     intent(in) :: domain
integer, intent(out), optional :: xbegin, xend, ybegin, yend, xsize, xmax_size, ysize, ymax_size
integer, intent(in),  optional :: tile_count, position
 end subroutine mpp_get_global_domain

 subroutine mpp_get_pelist( domain, pelist, pos )
  type(domain2D),     intent(in) :: domain
  integer,           intent(out) :: pelist(:)
  integer, intent(out), optional :: pos
end subroutine mpp_get_pelist

subroutine mpp_get_compute_domains( domain, xbegin, xend, xsize, ybegin, yend, ysize, position )
  type(domain2D),                   intent(in) :: domain
  integer, intent(out), optional, dimension(:) :: xbegin, xend, xsize, ybegin, yend, ysize
  integer, intent(in ), optional               :: position
end subroutine mpp_get_compute_domains

 integer function mpp_get_tile_id(domain)
  type(domain2D),       intent(inout)        :: domain
 end function mpp_get_tile_id

 subroutine mpp_copy_domain(domain, domain_for_read)
  type(domain2D),       intent(inout)        :: domain, domain_for_read
 end subroutine mpp_copy_domain

 subroutine mpp_get_layout(domain, layout)
  type(domain2D),       intent(in)        :: domain
  integer, intent(inout) :: layout(2)
 end subroutine mpp_get_layout

 function mpp_get_io_domain_layout(domain)
  type(domain2D),       intent(inout)        :: domain
  integer :: mpp_get_io_domain_layout(2)
 end function mpp_get_io_domain_layout

 subroutine mpp_reset_group_update_field_r4_2d(group, field)
type(mpp_group_update_type), intent(inout) :: group
real(kind=4),            intent(inout)        :: field(:,:)

end subroutine mpp_reset_group_update_field_r4_2d

subroutine mpp_reset_group_update_field_r8_2d(group, field)
type(mpp_group_update_type), intent(inout) :: group
real(kind=8),            intent(inout)        :: field(:,:)

end subroutine mpp_reset_group_update_field_r8_2d

subroutine mpp_reset_group_update_field_r4_3d(group, field)
  type(mpp_group_update_type), intent(inout) :: group
  real(kind=4),            intent(inout)        :: field(:,:,:)
  
  end subroutine mpp_reset_group_update_field_r4_3d
  
  subroutine mpp_reset_group_update_field_r8_3d(group, field)
  type(mpp_group_update_type), intent(inout) :: group
  real(kind=8),            intent(inout)        :: field(:,:,:)
  
  end subroutine mpp_reset_group_update_field_r8_3d

subroutine mpp_reset_group_update_field_r4_4d(group, field)
type(mpp_group_update_type), intent(inout) :: group
real(kind=4),            intent(inout)        :: field(:,:,:,:)

end subroutine mpp_reset_group_update_field_r4_4d

subroutine mpp_reset_group_update_field_r8_4d(group, field)
type(mpp_group_update_type), intent(inout) :: group
real(kind=8),            intent(inout)        :: field(:,:,:,:)

end subroutine mpp_reset_group_update_field_r8_4d

subroutine mpp_reset_group_update_field_r4_2dv( group, fieldx, fieldy)

type(mpp_group_update_type), intent(inout) :: group
real(kind=4),        intent(inout)            :: fieldx(:,:), fieldy(:,:)

end subroutine mpp_reset_group_update_field_r4_2dv

subroutine mpp_reset_group_update_field_r8_2dv( group, fieldx, fieldy)

type(mpp_group_update_type), intent(inout) :: group
real(kind=8),        intent(inout)            :: fieldx(:,:), fieldy(:,:)

end subroutine mpp_reset_group_update_field_r8_2dv

subroutine mpp_reset_group_update_field_r4_3dv( group, fieldx, fieldy)

type(mpp_group_update_type), intent(inout) :: group
real(kind=4),        intent(inout)            :: fieldx(:,:,:), fieldy(:,:,:)

end subroutine mpp_reset_group_update_field_r4_3dv

subroutine mpp_reset_group_update_field_r8_3dv( group, fieldx, fieldy)

type(mpp_group_update_type), intent(inout) :: group
real(kind=8),        intent(inout)            :: fieldx(:,:,:), fieldy(:,:,:)

end subroutine mpp_reset_group_update_field_r8_3dv

subroutine mpp_create_group_update_r4_2d(group, field, domain, flags, position, &
  whalo, ehalo, shalo, nhalo)
type(mpp_group_update_type), intent(inout) :: group
real(kind=4),            intent(inout)        :: field(:,:)
type(domain2D),       intent(inout)        :: domain
integer,              intent(in), optional :: flags
integer,              intent(in), optional :: position
integer,              intent(in), optional :: whalo, ehalo, shalo, nhalo

end subroutine mpp_create_group_update_r4_2d

subroutine mpp_create_group_update_r8_2d(group, field, domain, flags, position, &
  whalo, ehalo, shalo, nhalo)
type(mpp_group_update_type), intent(inout) :: group
real(kind=8),            intent(inout)        :: field(:,:)
type(domain2D),       intent(inout)        :: domain
integer,              intent(in), optional :: flags
integer,              intent(in), optional :: position
integer,              intent(in), optional :: whalo, ehalo, shalo, nhalo

end subroutine mpp_create_group_update_r8_2d

subroutine mpp_create_group_update_r4_3d(group, field, domain, flags, position, &
  whalo, ehalo, shalo, nhalo)
type(mpp_group_update_type), intent(inout) :: group
real(kind=4),            intent(inout)        :: field(:,:,:)
type(domain2D),       intent(inout)        :: domain
integer,              intent(in), optional :: flags
integer,              intent(in), optional :: position
integer,              intent(in), optional :: whalo, ehalo, shalo, nhalo

end subroutine mpp_create_group_update_r4_3d

subroutine mpp_create_group_update_r8_3d(group, field, domain, flags, position, &
  whalo, ehalo, shalo, nhalo)
type(mpp_group_update_type), intent(inout) :: group
real(kind=8),            intent(inout)        :: field(:,:,:)
type(domain2D),       intent(inout)        :: domain
integer,              intent(in), optional :: flags
integer,              intent(in), optional :: position
integer,              intent(in), optional :: whalo, ehalo, shalo, nhalo

end subroutine mpp_create_group_update_r8_3d

subroutine mpp_create_group_update_r4_4d(group, field, domain, flags, position, &
  whalo, ehalo, shalo, nhalo)
type(mpp_group_update_type), intent(inout) :: group
real(kind=4),            intent(inout)        :: field(:,:,:,:)
type(domain2D),       intent(inout)        :: domain
integer,              intent(in), optional :: flags
integer,              intent(in), optional :: position
integer,              intent(in), optional :: whalo, ehalo, shalo, nhalo

end subroutine mpp_create_group_update_r4_4d

subroutine mpp_create_group_update_r8_4d(group, field, domain, flags, position, &
  whalo, ehalo, shalo, nhalo)
type(mpp_group_update_type), intent(inout) :: group
real(kind=8),            intent(inout)        :: field(:,:,:,:)
type(domain2D),       intent(inout)        :: domain
integer,              intent(in), optional :: flags
integer,              intent(in), optional :: position
integer,              intent(in), optional :: whalo, ehalo, shalo, nhalo

end subroutine mpp_create_group_update_r8_4d

subroutine mpp_create_group_update_r4_2dv( group, fieldx, fieldy, domain, flags, gridtype, &
  whalo, ehalo, shalo, nhalo)

type(mpp_group_update_type), intent(inout) :: group
real(kind=4),        intent(inout)            :: fieldx(:,:), fieldy(:,:)
type(domain2D),   intent(inout)            :: domain
integer,          intent(in),     optional :: flags, gridtype
integer,          intent(in),     optional :: whalo, ehalo, shalo, nhalo

end subroutine mpp_create_group_update_r4_2dv

subroutine mpp_create_group_update_r8_2dv( group, fieldx, fieldy, domain, flags, gridtype, &
  whalo, ehalo, shalo, nhalo)

type(mpp_group_update_type), intent(inout) :: group
real(kind=8),        intent(inout)            :: fieldx(:,:), fieldy(:,:)
type(domain2D),   intent(inout)            :: domain
integer,          intent(in),     optional :: flags, gridtype
integer,          intent(in),     optional :: whalo, ehalo, shalo, nhalo

end subroutine mpp_create_group_update_r8_2dv

subroutine mpp_create_group_update_r4_3dv( group, fieldx, fieldy, domain, flags, gridtype, &
  whalo, ehalo, shalo, nhalo)

type(mpp_group_update_type), intent(inout) :: group
real(kind=4),        intent(inout)            :: fieldx(:,:,:), fieldy(:,:,:)
type(domain2D),   intent(inout)            :: domain
integer,          intent(in),     optional :: flags, gridtype
integer,          intent(in),     optional :: whalo, ehalo, shalo, nhalo

end subroutine mpp_create_group_update_r4_3dv

subroutine mpp_create_group_update_r8_3dv( group, fieldx, fieldy, domain, flags, gridtype, &
  whalo, ehalo, shalo, nhalo)

type(mpp_group_update_type), intent(inout) :: group
real(kind=8),        intent(inout)            :: fieldx(:,:,:), fieldy(:,:,:)
type(domain2D),   intent(inout)            :: domain
integer,          intent(in),     optional :: flags, gridtype
integer,          intent(in),     optional :: whalo, ehalo, shalo, nhalo

end subroutine mpp_create_group_update_r8_3dv

 subroutine mpp_do_group_update(group, domain, d_type)
  type(mpp_group_update_type), intent(in) :: group
  type(domain2d),               intent(inout) :: domain
  real                                        :: d_type
 end subroutine

 function mpp_group_update_initialized(group)
  type(mpp_group_update_type), intent(in) :: group
  logical :: mpp_group_update_initialized

end function mpp_group_update_initialized

 subroutine mpp_define_mosaic( global_indices, layout, domain, num_tile, num_contact, tile1, tile2,      &
  istart1, iend1, jstart1, jend1, istart2, iend2, jstart2, jend2, pe_start, &
  pe_end, pelist, whalo, ehalo, shalo, nhalo, xextent, yextent,             &
  maskmap, name, memory_size, symmetry, xflags, yflags, tile_id )
integer,          intent(in)           :: global_indices(:,:)  !>The size of first indice is 4,
                                     !! (/ isg, ieg, jsg, jeg /)
                                     !!The size of second indice
                                     !!is number of tiles in mosaic.
integer,          intent(in)           :: layout(:,:)
type(domain2D),   intent(inout)        :: domain
integer,          intent(in)           :: num_tile             !< number of tiles in the mosaic
integer,          intent(in)           :: num_contact          !< number of contact region between tiles.
integer,          intent(in)           :: tile1(:), tile2(:)   !< tile number
integer,          intent(in)           :: istart1(:), iend1(:) !< i-index in tile_1 of contact region
integer,          intent(in)           :: jstart1(:), jend1(:) !< j-index in tile_1 of contact region
integer,          intent(in)           :: istart2(:), iend2(:) !< i-index in tile_2 of contact region
integer,          intent(in)           :: jstart2(:), jend2(:) !< j-index in tile_2 of contact region
integer,          intent(in)           :: pe_start(:)          !< start pe of the pelist used in each tile
integer,          intent(in)           :: pe_end(:)            !< end pe of the pelist used in each tile
integer,          intent(in), optional :: pelist(:)            !< list of processors used in mosaic
integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
integer,          intent(in), optional :: xextent(:,:), yextent(:,:)
logical,          intent(in), optional :: maskmap(:,:,:)
character(len=*), intent(in), optional :: name
integer,          intent(in), optional :: memory_size(2)
logical,          intent(in), optional :: symmetry
integer,          intent(in), optional :: xflags, yflags
integer,          intent(in), optional :: tile_id(:)           !< tile_id of each tile in the mosaic

end subroutine

 subroutine mpp_start_group_update(g,d,d_type)
  type(mpp_group_update_type), intent(inout) :: g !< The data type that store information for group update
  type(domain2D),               intent(inout) :: d !< contains domain information
  real                                   , intent(in)     :: d_type
 end subroutine mpp_start_group_update

 subroutine mpp_complete_group_update(g,d,d_type)
  type(mpp_group_update_type), intent(inout) :: g !< The data type that store information for group update
  type(domain2D),               intent(inout) :: d !< contains domain information
  real                                   , intent(in)     :: d_type
 end subroutine mpp_complete_group_update

 subroutine mpp_define_io_domain(domain, layout)
  type(domain2D), intent(inout) :: domain !< Input 2D domain
  integer,        intent(in   ) :: layout(2) !< 2 value io pe layout to define
 end subroutine mpp_define_io_domain

 subroutine mpp_define_mosaic_pelist( sizes, pe_start, pe_end, pelist, costpertile)
  integer, dimension(:), intent(in)           :: sizes
  integer, dimension(:), intent(inout)        :: pe_start, pe_end
  integer, dimension(:), intent(in), optional :: pelist, costpertile
 end subroutine mpp_define_mosaic_pelist

 subroutine mpp_get_neighbor_pe(d, dir, pe)
  type(domain2d), intent(in) :: d
  integer, intent(in) :: dir
  integer, intent(out) :: pe
 end subroutine mpp_get_neighbor_pe

 subroutine mpp_define_layout(a,b,layout)
  integer, intent(IN) :: a(4)
  integer, intent(inout) :: b
  integer, intent(INOUT) :: layout(2)
  end subroutine mpp_define_layout

 subroutine mpp_broadcast_domain(domain)
  type(domain2d), intent(inout) :: domain
 end subroutine mpp_broadcast_domain

 subroutine mpp_domains_init(t)
  integer, intent(in) :: t
 end subroutine mpp_domains_init

 function mpp_global_min_r4(d, a)
  type(domain2d), intent(in) :: d
  real(kind=4), intent(in), dimension(:,:) :: a
  real(kind=4) :: mpp_global_min_r4
 end function mpp_global_min_r4

 function mpp_global_min_r8(d, a)
  type(domain2d), intent(in) :: d
  real(kind=8), intent(in), dimension(:,:) :: a
  real(kind=8) :: mpp_global_min_r8
 end function mpp_global_min_r8  

 function mpp_global_max_r4(d, a)
  type(domain2d), intent(in) :: d
  real(kind=4), dimension(:,:) :: a
  real(kind=4) :: mpp_global_max_r4
 end function mpp_global_max_r4

 function mpp_global_max_r8(d, a)
  type(domain2d), intent(in) :: d
  real(kind=8), intent(in), dimension(:,:) :: a
  real(kind=8) :: mpp_global_max_r8
 end function mpp_global_max_r8

integer function mpp_get_ntile_count(d)
type(domain2d)     :: d
end function mpp_get_ntile_count

! mpp_update_domains
! ------------------

 subroutine mpp_update_domain2D_r4_2d( field, domain, flags, complete, position, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=4),        intent(inout)        :: field(:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: position
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r4_2d
 subroutine mpp_update_domain2D_r4_3d(field, domain, flags, complete, position, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=4),        intent(inout)        :: field(:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: position
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r4_3d
 subroutine mpp_update_domain2D_r4_4d(field, domain, flags, complete, position, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=4),        intent(inout)        :: field(:,:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: position
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r4_4d
 subroutine mpp_update_domain2D_r4_5d(field, domain, flags, complete, position, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=4),        intent(inout)        :: field(:,:,:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: position
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r4_5d
 subroutine mpp_update_domain2D_r4_2dv(fieldx, fieldy, domain, flags, gridtype, complete, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=4),        intent(inout)        :: fieldx(:,:), fieldy(:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags, gridtype
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r4_2dv
 subroutine mpp_update_domain2D_r4_3dv(fieldx, fieldy, domain, flags, gridtype, complete, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=4),        intent(inout)        :: fieldx(:,:,:), fieldy(:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags, gridtype
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r4_3dv
 subroutine mpp_update_domain2D_r4_4dv(fieldx, fieldy, domain, flags, gridtype, complete, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=4),        intent(inout)        :: fieldx(:,:,:,:), fieldy(:,:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags, gridtype
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r4_4dv
 subroutine mpp_update_domain2D_r4_5dv(fieldx, fieldy, domain, flags, gridtype, complete, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=4),        intent(inout)        :: fieldx(:,:,:,:,:), fieldy(:,:,:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags, gridtype
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r4_5dv

 subroutine mpp_update_domain2D_r8_2d( field, domain, flags, complete, position, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=8),        intent(inout)        :: field(:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: position
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r8_2d
 subroutine mpp_update_domain2D_r8_3d(field, domain, flags, complete, position, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=8),        intent(inout)        :: field(:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: position
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r8_3d
 subroutine mpp_update_domain2D_r8_4d(field, domain, flags, complete, position, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=8),        intent(inout)        :: field(:,:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: position
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r8_4d
 subroutine mpp_update_domain2D_r8_5d(field, domain, flags, complete, position, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=8),        intent(inout)        :: field(:,:,:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: position
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r8_5d
 subroutine mpp_update_domain2D_r8_2dv(fieldx, fieldy, domain, flags, gridtype, complete, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=8),        intent(inout)        :: fieldx(:,:), fieldy(:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags, gridtype
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r8_2dv
 subroutine mpp_update_domain2D_r8_3dv(fieldx, fieldy, domain, flags, gridtype, complete, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=8),        intent(inout)        :: fieldx(:,:,:), fieldy(:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags, gridtype
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r8_3dv
 subroutine mpp_update_domain2D_r8_4dv(fieldx, fieldy, domain, flags, gridtype, complete, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=8),        intent(inout)        :: fieldx(:,:,:,:), fieldy(:,:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags, gridtype
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r8_4dv
 subroutine mpp_update_domain2D_r8_5dv(fieldx, fieldy, domain, flags, gridtype, complete, &
  whalo, ehalo, shalo, nhalo, name, tile_count)
    real(kind=8),        intent(inout)        :: fieldx(:,:,:,:,:), fieldy(:,:,:,:,:)
    type(domain2D),   intent(inout)        :: domain
    integer,          intent(in), optional :: flags, gridtype
    logical,          intent(in), optional :: complete
    integer,          intent(in), optional :: whalo, ehalo, shalo, nhalo
    character(len=*), intent(in), optional :: name
    integer,          intent(in), optional :: tile_count
 endsubroutine mpp_update_domain2D_r8_5dv



! mpp_get_boundary
! ----------------

 subroutine mpp_get_boundary_r4_2d(field1,field2,domain,wbuffery,ebuffery,sbuffery,nbuffery,wbufferx,ebufferx,sbufferx,nbufferx,gridtype,flags)
  real(kind=4), intent(inout) :: field1(:,:),field2(:,:)
  type(domain2d), intent(in) :: domain
  real(kind=4), optional, intent(inout) :: wbuffery(:),ebuffery(:),sbuffery(:),nbuffery(:)
  real(kind=4), optional, intent(inout) :: wbufferx(:),ebufferx(:),sbufferx(:),nbufferx(:)
  integer, optional, intent(in) :: gridtype
  integer, optional, intent(in) :: flags
 endsubroutine mpp_get_boundary_r4_2d

 subroutine mpp_get_boundary_r8_2d(field1,field2,domain,wbuffery,ebuffery,sbuffery,nbuffery,wbufferx,ebufferx,sbufferx,nbufferx,gridtype,flags)
  real(kind=8), intent(inout) :: field1(:,:),field2(:,:)
  type(domain2d), intent(in) :: domain
  real(kind=8), optional, intent(inout) :: wbuffery(:),ebuffery(:),sbuffery(:),nbuffery(:)
  real(kind=8), optional, intent(inout) :: wbufferx(:),ebufferx(:),sbufferx(:),nbufferx(:)
  integer, optional, intent(in) :: gridtype
  integer, optional, intent(in) :: flags
 endsubroutine mpp_get_boundary_r8_2d


 subroutine mpp_get_boundary_r4_3d(field1,field2,domain,wbuffery,ebuffery,sbuffery,nbuffery,wbufferx,ebufferx,sbufferx,nbufferx,gridtype,flags)
  real(kind=4), intent(inout) :: field1(:,:,:),field2(:,:,:)
  type(domain2d), intent(in) :: domain
  real(kind=4), optional, intent(inout) :: wbuffery(:,:),ebuffery(:,:),sbuffery(:,:),nbuffery(:,:)
  real(kind=4), optional, intent(inout) :: wbufferx(:,:),ebufferx(:,:),sbufferx(:,:),nbufferx(:,:)
  integer, optional, intent(in) :: gridtype
  integer, optional, intent(in) :: flags
 endsubroutine mpp_get_boundary_r4_3d

 subroutine mpp_get_boundary_r8_3d(field1,field2,domain,wbuffery,ebuffery,sbuffery,nbuffery,wbufferx,ebufferx,sbufferx,nbufferx,gridtype,flags)
  real(kind=8), intent(inout) :: field1(:,:,:),field2(:,:,:)
  type(domain2d), intent(in) :: domain
  real(kind=8), optional, intent(inout) :: wbuffery(:,:),ebuffery(:,:),sbuffery(:,:),nbuffery(:,:)
  real(kind=8), optional, intent(inout) :: wbufferx(:,:),ebufferx(:,:),sbufferx(:,:),nbufferx(:,:)
  integer, optional, intent(in) :: gridtype
  integer, optional, intent(in) :: flags
 endsubroutine mpp_get_boundary_r8_3d



! mpp_get_data_domain
! -------------------

 subroutine mpp_get_data_domain(domain,isd,ied,jsd,jed)

  type(domain2d), intent(in) :: domain
  integer, intent(in) :: isd,ied,jsd,jed

 endsubroutine mpp_get_data_domain



! mpp_get_compute_domain
! ----------------------
 subroutine mpp_get_compute_domain(domain,isd,ied,jsd,jed)

  type(domain2d), intent(in) :: domain
  integer, intent(in) :: isd,ied,jsd,jed

 endsubroutine mpp_get_compute_domain



! mpp_global_field
! ----------------
 subroutine mpp_global_field2d_r4_2d(domain,field_this_grid,field,position)
  type(domain2D), intent(in) :: domain
  real(kind=4), intent(in) :: field_this_grid(:,:)
  real(kind=4), intent(inout) :: field(:,:)
  integer, intent(in), optional :: position
 endsubroutine mpp_global_field2d_r4_2d
 subroutine mpp_global_field2d_r4_3d(domain,field_this_grid,field,position)
  type(domain2D), intent(in) :: domain
  real(kind=4), intent(in) :: field_this_grid(:,:,:)
  real(kind=4), intent(inout) :: field(:,:,:)
  integer, intent(in), optional :: position
 endsubroutine mpp_global_field2d_r4_3d
 subroutine mpp_global_field2d_r4_4d(domain,field_this_grid,field,position)
  type(domain2D), intent(in) :: domain
  real(kind=4), intent(in) :: field_this_grid(:,:,:,:)
  real(kind=4), intent(inout) :: field(:,:,:,:)
  integer, intent(in), optional :: position
 endsubroutine mpp_global_field2d_r4_4d
 subroutine mpp_global_field2d_r4_5d(domain,field_this_grid,field,position)
  type(domain2D), intent(in) :: domain
  real(kind=4), intent(in) :: field_this_grid(:,:,:,:,:)
  real(kind=4), intent(inout) :: field(:,:,:,:,:)
  integer, intent(in), optional :: position
 endsubroutine mpp_global_field2d_r4_5d

 subroutine mpp_global_field2d_r8_2d(domain,field_this_grid,field,position)
  type(domain2D), intent(in) :: domain
  real(kind=8), intent(in) :: field_this_grid(:,:)
  real(kind=8), intent(inout) :: field(:,:)
  integer, intent(in), optional :: position
 endsubroutine mpp_global_field2d_r8_2d
 subroutine mpp_global_field2d_r8_3d(domain,field_this_grid,field,position)
  type(domain2D), intent(in) :: domain
  real(kind=8), intent(in) :: field_this_grid(:,:,:)
  real(kind=8), intent(inout) :: field(:,:,:)
  integer, intent(in), optional :: position
 endsubroutine mpp_global_field2d_r8_3d
 subroutine mpp_global_field2d_r8_4d(domain,field_this_grid,field,position)
  type(domain2D), intent(in) :: domain
  real(kind=8), intent(in) :: field_this_grid(:,:,:,:)
  real(kind=8), intent(inout) :: field(:,:,:,:)
  integer, intent(in), optional :: position
 endsubroutine mpp_global_field2d_r8_4d
 subroutine mpp_global_field2d_r8_5d(domain,field_this_grid,field,position)
  type(domain2D), intent(in) :: domain
  real(kind=8), intent(in) :: field_this_grid(:,:,:,:,:)
  real(kind=8), intent(inout) :: field(:,:,:,:,:)
  integer, intent(in), optional :: position
 endsubroutine mpp_global_field2d_r8_5d



! mpp_get_C2F_index
! -----------------

 subroutine mpp_get_C2F_index(nest_domain, is_fine, ie_fine, js_fine, je_fine, &
  is_coarse, ie_coarse, js_coarse, je_coarse, dir, nest_level, position)
type(nest_domain_type), intent(in ) :: nest_domain 
integer,                intent(out) :: is_fine, ie_fine, js_fine, je_fine
integer,                intent(out) :: is_coarse, ie_coarse, js_coarse, je_coarse 
integer,                intent(in ) :: dir, nest_level 
integer, optional,      intent(in ) :: position 
end subroutine mpp_get_C2F_index

! mpp_get_F2C_index
! ---------------

subroutine mpp_get_F2C_index(nest_domain, is_coarse, ie_coarse, js_coarse, je_coarse, &
  is_fine, ie_fine, js_fine, je_fine, nest_level, position)
type(nest_domain_type), intent(in ) :: nest_domain
integer,                intent(out) :: is_fine, ie_fine, js_fine, je_fine
integer,                intent(out) :: is_coarse, ie_coarse, js_coarse, je_coarse 
integer,                intent(in)  :: nest_level
integer, optional,      intent(in ) :: position
 endsubroutine mpp_get_F2C_index



! mpp_update_nest_fine
! --------------------

 subroutine mpp_update_nest_fine_r4_2d(field, nest_domain, wbuffer, ebuffer, sbuffer, nbuffer, &
  nest_level, flags, complete, position, extra_halo, name, tile_count)
real(kind=4),             intent(in)      :: field(:,:)
type(nest_domain_type), intent(inout)  :: nest_domain
real(kind=4),             intent(inout)   :: wbuffer(:,:) 
real(kind=4),             intent(inout)   :: ebuffer(:,:) 
real(kind=4),             intent(inout)   :: sbuffer(:,:) 
real(kind=4),             intent(inout)   :: nbuffer(:,:) 
integer,          intent(in)           :: nest_level
integer,          intent(in), optional :: flags 
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position 
integer,          intent(in), optional :: extra_halo 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count
end subroutine mpp_update_nest_fine_r4_2d

subroutine mpp_update_nest_fine_r4_3d(field, nest_domain, wbuffer, sbuffer, ebuffer, nbuffer, &
  nest_level, flags, complete, position, extra_halo, name, tile_count)
real(kind=4),             intent(in)      :: field(:,:,:) 
type(nest_domain_type), intent(inout)  :: nest_domain 
real(kind=4),             intent(inout)   :: wbuffer(:,:,:) 
real(kind=4),             intent(inout)   :: ebuffer(:,:,:) 
real(kind=4),             intent(inout)   :: sbuffer(:,:,:) 
real(kind=4),             intent(inout)   :: nbuffer(:,:,:) 
integer,          intent(in)           :: nest_level 
integer,          intent(in), optional :: flags
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position 
integer,          intent(in), optional :: extra_halo 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count
end subroutine mpp_update_nest_fine_r4_3d

subroutine mpp_update_nest_fine_r4_4d(field, nest_domain, wbuffer, ebuffer, sbuffer, nbuffer, &
  nest_level, flags, complete, position, extra_halo, name, tile_count)
real(kind=4),             intent(in)      :: field(:,:,:,:) 
type(nest_domain_type), intent(inout)  :: nest_domain 
real(kind=4),             intent(inout)   :: wbuffer(:,:,:,:) 
real(kind=4),             intent(inout)   :: ebuffer(:,:,:,:) 
real(kind=4),             intent(inout)   :: sbuffer(:,:,:,:) 
real(kind=4),             intent(inout)   :: nbuffer(:,:,:,:) 
integer,          intent(in)           :: nest_level 
integer,          intent(in), optional :: flags  
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position 
integer,          intent(in), optional :: extra_halo 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count 
end subroutine mpp_update_nest_fine_r4_4d

subroutine mpp_update_nest_fine_r4_2d_v(fieldx, fieldy, nest_domain, wbufferx, wbuffery, sbufferx, sbuffery, &
  ebufferx, ebuffery, nbufferx, nbuffery, nest_level, &
  flags, gridtype, complete, extra_halo, name, tile_count)
real(kind=4),             intent(in)      :: fieldx(:,:), fieldy(:,:) 
type(nest_domain_type), intent(inout)  :: nest_domain 
real(kind=4),             intent(inout)   :: wbufferx(:,:), wbuffery(:,:) 
real(kind=4),             intent(inout)   :: ebufferx(:,:), ebuffery(:,:) 
real(kind=4),             intent(inout)   :: sbufferx(:,:), sbuffery(:,:) 
real(kind=4),             intent(inout)   :: nbufferx(:,:), nbuffery(:,:) 
integer,          intent(in)           :: nest_level
integer,          intent(in), optional :: flags
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: gridtype
integer,          intent(in), optional :: extra_halo 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count 
end subroutine mpp_update_nest_fine_r4_2d_v

subroutine mpp_update_nest_fine_r4_3d_v(fieldx, fieldy, nest_domain, wbufferx, wbuffery, sbufferx, sbuffery, &
  ebufferx, ebuffery, nbufferx, nbuffery, nest_level, &
  flags, gridtype, complete, extra_halo, name, tile_count)
real(kind=4),             intent(in)      :: fieldx(:,:,:), fieldy(:,:,:) 
type(nest_domain_type), intent(inout)  :: nest_domain
real(kind=4),             intent(inout)   :: wbufferx(:,:,:), wbuffery(:,:,:) 
real(kind=4),             intent(inout)   :: ebufferx(:,:,:), ebuffery(:,:,:) 
real(kind=4),             intent(inout)   :: sbufferx(:,:,:), sbuffery(:,:,:) 
real(kind=4),             intent(inout)   :: nbufferx(:,:,:), nbuffery(:,:,:) 
integer,          intent(in)           :: nest_level 
integer,          intent(in), optional :: flags 
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: gridtype
integer,          intent(in), optional :: extra_halo
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count
end subroutine mpp_update_nest_fine_r4_3d_v

subroutine mpp_update_nest_fine_r8_2d(field, nest_domain, wbuffer, ebuffer, sbuffer, nbuffer, &
  nest_level, flags, complete, position, extra_halo, name, tile_count)
real(kind=8),             intent(in)      :: field(:,:)
type(nest_domain_type), intent(inout)  :: nest_domain
real(kind=8),             intent(inout)   :: wbuffer(:,:) 
real(kind=8),             intent(inout)   :: ebuffer(:,:) 
real(kind=8),             intent(inout)   :: sbuffer(:,:) 
real(kind=8),             intent(inout)   :: nbuffer(:,:) 
integer,          intent(in)           :: nest_level
integer,          intent(in), optional :: flags 
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position 
integer,          intent(in), optional :: extra_halo 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count
end subroutine mpp_update_nest_fine_r8_2d

subroutine mpp_update_nest_fine_r8_3d(field, nest_domain, wbuffer, sbuffer, ebuffer, nbuffer, &
  nest_level, flags, complete, position, extra_halo, name, tile_count)
real(kind=8),             intent(in)      :: field(:,:,:) 
type(nest_domain_type), intent(inout)  :: nest_domain 
real(kind=8),             intent(inout)   :: wbuffer(:,:,:) 
real(kind=8),             intent(inout)   :: ebuffer(:,:,:) 
real(kind=8),             intent(inout)   :: sbuffer(:,:,:) 
real(kind=8),             intent(inout)   :: nbuffer(:,:,:) 
integer,          intent(in)           :: nest_level 
integer,          intent(in), optional :: flags
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position 
integer,          intent(in), optional :: extra_halo 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count
end subroutine mpp_update_nest_fine_r8_3d

subroutine mpp_update_nest_fine_r8_4d(field, nest_domain, wbuffer, ebuffer, sbuffer, nbuffer, &
  nest_level, flags, complete, position, extra_halo, name, tile_count)
real(kind=8),             intent(in)      :: field(:,:,:,:) 
type(nest_domain_type), intent(inout)  :: nest_domain 
real(kind=8),             intent(inout)   :: wbuffer(:,:,:,:) 
real(kind=8),             intent(inout)   :: ebuffer(:,:,:,:) 
real(kind=8),             intent(inout)   :: sbuffer(:,:,:,:) 
real(kind=8),             intent(inout)   :: nbuffer(:,:,:,:) 
integer,          intent(in)           :: nest_level 
integer,          intent(in), optional :: flags  
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position 
integer,          intent(in), optional :: extra_halo 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count 
end subroutine mpp_update_nest_fine_r8_4d

subroutine mpp_update_nest_fine_r8_2d_v(fieldx, fieldy, nest_domain, wbufferx, wbuffery, sbufferx, sbuffery, &
  ebufferx, ebuffery, nbufferx, nbuffery, nest_level, &
  flags, gridtype, complete, extra_halo, name, tile_count)
real(kind=8),             intent(in)      :: fieldx(:,:), fieldy(:,:) 
type(nest_domain_type), intent(inout)  :: nest_domain 
real(kind=8),             intent(inout)   :: wbufferx(:,:), wbuffery(:,:) 
real(kind=8),             intent(inout)   :: ebufferx(:,:), ebuffery(:,:) 
real(kind=8),             intent(inout)   :: sbufferx(:,:), sbuffery(:,:) 
real(kind=8),             intent(inout)   :: nbufferx(:,:), nbuffery(:,:) 
integer,          intent(in)           :: nest_level
integer,          intent(in), optional :: flags
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: gridtype
integer,          intent(in), optional :: extra_halo 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count 
end subroutine mpp_update_nest_fine_r8_2d_v

subroutine mpp_update_nest_fine_r8_3d_v(fieldx, fieldy, nest_domain, wbufferx, wbuffery, sbufferx, sbuffery, &
  ebufferx, ebuffery, nbufferx, nbuffery, nest_level, &
  flags, gridtype, complete, extra_halo, name, tile_count)
real(kind=8),             intent(in)      :: fieldx(:,:,:), fieldy(:,:,:) 
type(nest_domain_type), intent(inout)  :: nest_domain
real(kind=8),             intent(inout)   :: wbufferx(:,:,:), wbuffery(:,:,:) 
real(kind=8),             intent(inout)   :: ebufferx(:,:,:), ebuffery(:,:,:) 
real(kind=8),             intent(inout)   :: sbufferx(:,:,:), sbuffery(:,:,:) 
real(kind=8),             intent(inout)   :: nbufferx(:,:,:), nbuffery(:,:,:) 
integer,          intent(in)           :: nest_level 
integer,          intent(in), optional :: flags 
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: gridtype
integer,          intent(in), optional :: extra_halo
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count
end subroutine mpp_update_nest_fine_r8_3d_v


! mpp_update_nest_coarse
! ----------------------

subroutine mpp_update_nest_coarse_r4_2d(field_in, nest_domain, field_out, nest_level, complete, position, name, &
  &  tile_count)
real(kind=4),             intent(in)      :: field_in(:,:)
type(nest_domain_type), intent(inout)  :: nest_domain 
real(kind=4),             intent(inout)   :: field_out(:,:) 
integer,          intent(in)           :: nest_level 
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count 
end subroutine mpp_update_nest_coarse_r4_2d

subroutine mpp_update_nest_coarse_r4_3d(field_in, nest_domain, field_out, nest_level, complete, position, name, &
  &  tile_count)
real(kind=4),             intent(in)      :: field_in(:,:,:)
type(nest_domain_type), intent(inout)  :: nest_domain 
real(kind=4),             intent(inout)   :: field_out(:,:,:) 
integer,          intent(in)           :: nest_level
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count
end subroutine mpp_update_nest_coarse_r4_3d

subroutine mpp_update_nest_coarse_r4_4d(field_in, nest_domain, field_out, nest_level, complete, position, name, &
  &  tile_count)
real(kind=4),             intent(in)      :: field_in(:,:,:,:)
type(nest_domain_type), intent(inout)  :: nest_domain
real(kind=4),             intent(inout)   :: field_out(:,:,:,:) 
integer,          intent(in)           :: nest_level 
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count 
end subroutine mpp_update_nest_coarse_r4_4d

subroutine mpp_update_nest_coarse_r4_3d_v(fieldx_in, fieldy_in, nest_domain, fieldx_out, fieldy_out, nest_level, &
  flags, gridtype, complete, name, tile_count)
real(kind=4),             intent(in)      :: fieldx_in(:,:,:) 
real(kind=4),             intent(in)      :: fieldy_in(:,:,:) 
type(nest_domain_type), intent(inout)  :: nest_domain 
integer,          intent(in), optional :: flags, gridtype 
real(kind=4),             intent(inout)   :: fieldx_out(:,:,:) 
real(kind=4),             intent(inout)   :: fieldy_out(:,:,:) 
integer,          intent(in)           :: nest_level 
logical,          intent(in), optional :: complete
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count
end subroutine mpp_update_nest_coarse_r4_3d_v

subroutine mpp_update_nest_coarse_r8_2d(field_in, nest_domain, field_out, nest_level, complete, position, name, &
  &  tile_count)
real(kind=8),             intent(in)      :: field_in(:,:)
type(nest_domain_type), intent(inout)  :: nest_domain 
real(kind=8),             intent(inout)   :: field_out(:,:) 
integer,          intent(in)           :: nest_level 
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count 
end subroutine mpp_update_nest_coarse_r8_2d

subroutine mpp_update_nest_coarse_r8_3d(field_in, nest_domain, field_out, nest_level, complete, position, name, &
  &  tile_count)
real(kind=8),             intent(in)      :: field_in(:,:,:)
type(nest_domain_type), intent(inout)  :: nest_domain 
real(kind=8),             intent(inout)   :: field_out(:,:,:) 
integer,          intent(in)           :: nest_level
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count
end subroutine mpp_update_nest_coarse_r8_3d

subroutine mpp_update_nest_coarse_r8_4d(field_in, nest_domain, field_out, nest_level, complete, position, name, &
  &  tile_count)
real(kind=8),             intent(in)      :: field_in(:,:,:,:)
type(nest_domain_type), intent(inout)  :: nest_domain
real(kind=8),             intent(inout)   :: field_out(:,:,:,:) 
integer,          intent(in)           :: nest_level 
logical,          intent(in), optional :: complete 
integer,          intent(in), optional :: position 
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count 
end subroutine mpp_update_nest_coarse_r8_4d

subroutine mpp_update_nest_coarse_r8_3d_v(fieldx_in, fieldy_in, nest_domain, fieldx_out, fieldy_out, nest_level, &
  flags, gridtype, complete, name, tile_count)
real(kind=8),             intent(in)      :: fieldx_in(:,:,:) 
real(kind=8),             intent(in)      :: fieldy_in(:,:,:) 
type(nest_domain_type), intent(inout)  :: nest_domain 
integer,          intent(in), optional :: flags, gridtype 
real(kind=8),             intent(inout)   :: fieldx_out(:,:,:) 
real(kind=8),             intent(inout)   :: fieldy_out(:,:,:) 
integer,          intent(in)           :: nest_level 
logical,          intent(in), optional :: complete
character(len=*), intent(in), optional :: name 
integer,          intent(in), optional :: tile_count
end subroutine mpp_update_nest_coarse_r8_3d_v


! mpp_global_sum
! --------------

 real(kind=R_GRID) function mpp_global_sum_2d( domain, field, flags, position, tile_count) 

    type(domain2D), intent(in) :: domain
    real(R_GRID), intent(in) :: field(:,:)
    integer, intent(in), optional :: flags
    integer, intent(in), optional :: position
    integer, intent(in), optional :: tile_count

    mpp_global_sum_2d = sum(field)

 endfunction mpp_global_sum_2d

 real(kind=R_GRID) function mpp_global_sum_3d( domain, field, flags, position, tile_count) 

    type(domain2D), intent(in) :: domain
    real(R_GRID), intent(in) :: field(:,:,:)
    integer, intent(in), optional :: flags
    integer, intent(in), optional :: position
    integer, intent(in), optional :: tile_count

    mpp_global_sum_3d = sum(field)

 endfunction mpp_global_sum_3d

 real(kind=R_GRID) function mpp_global_sum_4d( domain, field, flags, position, tile_count) 

    type(domain2D), intent(in) :: domain
    real(R_GRID), intent(in) :: field(:,:,:,:)
    integer, intent(in), optional :: flags
    integer, intent(in), optional :: position
    integer, intent(in), optional :: tile_count

    mpp_global_sum_4d = sum(field)

 endfunction mpp_global_sum_4d

 real(kind=R_GRID) function mpp_global_sum_5d( domain, field, flags, position, tile_count) 

    type(domain2D), intent(in) :: domain
    real(R_GRID), intent(in) :: field(:,:,:,:,:)
    integer, intent(in), optional :: flags
    integer, intent(in), optional :: position
    integer, intent(in), optional :: tile_count

    mpp_global_sum_5d = sum(field)

 endfunction mpp_global_sum_5d


end module mpp_domains_mod

