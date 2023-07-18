module mpp_mod

  ! MPP_DEBUG, NOTE, MPP_CLOCK_SYNC,MPP_CLOCK_DETAILED, WARNING

use platform_mod, only: r8_kind, i8_kind
                             
implicit none
private

public mpp_error, FATAL, mpp_sum, mpp_sync, mpp_npes, mpp_broadcast, WARNING, mpp_pe, &
       mpp_send, mpp_recv, mpp_sync_self, mpp_max, mpp_root_pe, mpp_clock_begin, &
       mpp_clock_end, mpp_clock_id, lowercase, stdlog, MPP_DEBUG, NOTE, &
       MPP_CLOCK_SYNC,MPP_CLOCK_DETAILED, mpp_set_warn_level, mpp_declare_pelist, &
       mpp_set_current_pelist, mpp_chksum, stdout, stderr, EVENT_RECV, mpp_gather, &
       mpp_get_current_pelist, get_unit, CLOCK_ROUTINE, mpp_min, NULL_PE, mpp_transmit, &
       read_input_nml, read_ascii_file

integer, parameter :: NAME_LENGTH = 64
integer, parameter, public :: CLOCK_SUBCOMPONENT=11
integer, parameter :: NOTE=0, WARNING=1, FATAL=2
integer, parameter :: NULL_PE=-3
integer, parameter :: CLOCK_ROUTINE=41
integer, parameter :: EVENT_RECV=3
integer, parameter :: MPP_CLOCK_SYNC=1, MPP_CLOCK_DETAILED=2
integer, parameter :: MPP_VERBOSE=1, MPP_DEBUG=2
character(len=:), dimension(:), allocatable, target, public :: input_nml_file

integer :: mpp_transmit ! dummy
integer :: read_ascii_file ! dummy

interface mpp_chksum
    module procedure mpp_chksum_r4_2d
    module procedure mpp_chksum_r4_3d
    module procedure mpp_chksum_r4_4d
    module procedure mpp_chksum_r8_2d
    module procedure mpp_chksum_r8_3d
    module procedure mpp_chksum_r8_4d
end interface

interface mpp_gather
    module procedure mpp_gather_i4_1d
    module procedure mpp_gather_r4_1dv
    module procedure mpp_gather_r8_1dv
    module procedure mpp_gather_pelist_r4_2d
    module procedure mpp_gather_pelist_r8_2d
    module procedure mpp_gather_pelist_r4_3d
    module procedure mpp_gather_pelist_r8_3d
end interface

interface mpp_broadcast
    module procedure mpp_broadcast_l_0d
    module procedure mpp_broadcast_i4_0d
    module procedure mpp_broadcast_i4_1d
    module procedure mpp_broadcast_i8_0d
    module procedure mpp_broadcast_i8_1d
    module procedure mpp_broadcast_r4_0d
    module procedure mpp_broadcast_r4_1d
    module procedure mpp_broadcast_r4_2d
    module procedure mpp_broadcast_r4_3d
    module procedure mpp_broadcast_r4_4d
    module procedure mpp_broadcast_r8_0d
    module procedure mpp_broadcast_r8_1d
    module procedure mpp_broadcast_r8_2d
    module procedure mpp_broadcast_r8_3d
    module procedure mpp_broadcast_r8_4d
end interface

interface mpp_min
    module procedure mpp_min_r4
    module procedure mpp_min_r8
end interface

interface mpp_max
    module procedure mpp_max_i4_1d
    module procedure mpp_max_i8_1d
    module procedure mpp_max_r4
    module procedure mpp_max_r8
end interface

interface mpp_recv
    module procedure mpp_recv_r4_0d
    module procedure mpp_recv_r8_0d
    module procedure mpp_recv_r4_3d
    module procedure mpp_recv_r8_3d
    module procedure mpp_recv_i4_0d
    module procedure mpp_recv_i8_0d
end interface

interface mpp_send
    module procedure mpp_send_r4_0d
    module procedure mpp_send_r8_0d
    module procedure mpp_send_r4_3d
    module procedure mpp_send_r8_3d
    module procedure mpp_send_i4_0d
    module procedure mpp_send_i8_0d
end interface

interface mpp_sum
    module procedure mpp_sum_r4_0d
    module procedure mpp_sum_r8_0d
    module procedure mpp_sum_i4_0d
    module procedure mpp_sum_i8_0d
end interface

contains

subroutine read_input_nml(file)
  character(len=*), intent(in) :: file
end subroutine read_input_nml

subroutine mpp_sync_self(check)
  integer, optional, intent(in) :: check
end subroutine

subroutine mpp_max_i4_1d(arg, len, pelist)
  integer(kind=4), intent(inout) :: arg(:)
  integer, intent(in) :: len
  integer, optional, intent(in) :: pelist(:)
end subroutine mpp_max_i4_1d

subroutine mpp_max_i8_1d(arg, len, pelist)
  integer(kind=8), intent(inout) :: arg(:)
  integer, intent(in) :: len
  integer, optional, intent(in) :: pelist(:)
end subroutine mpp_max_i8_1d

subroutine mpp_max_r4(arg)
  real(kind=4), intent(inout) :: arg
end subroutine mpp_max_r4

subroutine mpp_max_r8(arg)
  real(kind=8), intent(inout) :: arg
end subroutine mpp_max_r8

subroutine mpp_min_r4(arg)
  real(kind=4), intent(inout) :: arg
end subroutine mpp_min_r4

subroutine mpp_min_r8(arg)
  real(kind=8), intent(inout) :: arg
end subroutine mpp_min_r8

integer function get_unit()
end function get_unit

subroutine mpp_broadcast_l_0d( data, length, from_pe, pelist )
  logical, intent(inout) :: data
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_l_0d

subroutine mpp_broadcast_i4_0d( data, length, from_pe, pelist )
  integer(kind=4), intent(inout) :: data
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_i4_0d

subroutine mpp_broadcast_i4_1d( data, length, from_pe, pelist )
  integer(kind=4), intent(inout) :: data(:)
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_i4_1d

subroutine mpp_broadcast_i8_0d( data, length, from_pe, pelist )
  integer(kind=8), intent(inout) :: data
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_i8_0d

subroutine mpp_broadcast_i8_1d( data, length, from_pe, pelist )
  integer(kind=8), intent(inout) :: data(:)
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_i8_1d

subroutine mpp_broadcast_r4_0d( data, length, from_pe, pelist )
  real(kind=4), intent(inout) :: data
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_r4_0d

subroutine mpp_broadcast_r4_1d( data, length, from_pe, pelist )
  real(kind=4), intent(inout) :: data(:)
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_r4_1d

subroutine mpp_broadcast_r4_2d( data, length, from_pe, pelist )
  real(kind=4), intent(inout) :: data(:,:)
  integer, intent(in) :: length, from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_r4_2d

subroutine mpp_broadcast_r4_3d( data, length, from_pe, pelist )
  real(kind=4), intent(inout) :: data(:,:,:)
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_r4_3d

subroutine mpp_broadcast_r4_4d( data, length, from_pe, pelist )
  real(kind=4), intent(inout) :: data(:,:,:,:)
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_r4_4d

subroutine mpp_broadcast_r8_0d( data, length, from_pe, pelist )
  real(kind=8), intent(inout) :: data
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_r8_0d

subroutine mpp_broadcast_r8_1d( data, length, from_pe, pelist )
  real(kind=8), intent(inout) :: data(:)
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_r8_1d

subroutine mpp_broadcast_r8_2d( data, length, from_pe, pelist )
  real(kind=8), intent(inout) :: data(:,:)
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_r8_2d

subroutine mpp_broadcast_r8_3d( data, length, from_pe, pelist )
  real(kind=8), intent(inout) :: data(:,:,:)
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_r8_3d

subroutine mpp_broadcast_r8_4d( data, length, from_pe, pelist )
  real(kind=8), intent(inout) :: data(:,:,:,:)
  integer, intent(in) :: length
  integer, optional, intent(in) :: from_pe
  integer, intent(in), optional :: pelist(:)
end subroutine mpp_broadcast_r8_4d

subroutine mpp_get_current_pelist( pelist, name, commID )
  integer, intent(out) :: pelist(:)
  character(len=*), intent(out), optional :: name
  integer, intent(out), optional :: commID
end subroutine mpp_get_current_pelist

integer function stdlog()
end function stdlog

integer function stdout()
end function stdout

integer function stderr()
end function stderr

function lowercase (cs)
  character(len=*), intent(in) :: cs
  character(len=len(cs)),target       :: lowercase
end function lowercase

function mpp_clock_id( name, flags, grain )
  integer                       :: mpp_clock_id
  character(len=*),  intent(in) :: name
  integer, intent(in), optional :: flags, grain
end function mpp_clock_id

subroutine mpp_clock_begin(id)
  integer, intent(in) :: id
end subroutine mpp_clock_begin

subroutine mpp_clock_end(id)
  integer, intent(in) :: id
end subroutine mpp_clock_end

integer function mpp_pe()
end function mpp_pe

integer function mpp_npes()
end function mpp_npes

integer function mpp_root_pe()
end function mpp_root_pe

 subroutine mpp_error(fatal,statement,int, str)
  integer, intent(in) :: fatal
  character(len=*), intent(in) :: statement
  integer, optional, intent(in) :: int
  character(len=*), optional, intent(in) :: str
 endsubroutine mpp_error

 subroutine mpp_sum_r4_0d(val,npes)
  real(kind=4), intent(in) :: val
  integer, optional, intent(in) :: npes
 endsubroutine mpp_sum_r4_0d

 subroutine mpp_sum_r8_0d(val,npes)
  real(kind=8), intent(in) :: val
  integer, optional, intent(in) :: npes
 endsubroutine mpp_sum_r8_0d

 subroutine mpp_sum_i4_0d(val,npes)
  integer(kind=4), intent(in) :: val
  integer, optional, intent(in) :: npes
 endsubroutine mpp_sum_i4_0d

 subroutine mpp_sum_i8_0d(val,npes)
  integer(kind=8), intent(in) :: val
  integer, optional, intent(in) :: npes
 endsubroutine mpp_sum_i8_0d

 subroutine mpp_send_r4_0d(field,plen,to_pe,block)
  real(kind=4), intent(inout) :: field
  integer, optional, intent(in) :: plen
  integer, optional, intent(in) :: to_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_send_r4_0d

 subroutine mpp_send_r8_0d(field,plen,to_pe,block)
  real(kind=8), intent(inout) :: field
  integer, optional, intent(in) :: plen
  integer, optional, intent(in) :: to_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_send_r8_0d

 subroutine mpp_send_r4_3d(field,plen,to_pe,block)
  real(kind=4), intent(inout) :: field(:,:,:)
  integer, optional, intent(in) :: plen
  integer, optional, intent(in) :: to_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_send_r4_3d

 subroutine mpp_send_r8_3d(field,plen,to_pe,block)
  real(kind=8), intent(inout) :: field(:,:,:)
  integer, optional, intent(in) :: plen
  integer, optional, intent(in) :: to_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_send_r8_3d

 subroutine mpp_send_i4_0d(field,plen,to_pe,block)
  integer(kind=4), intent(inout) :: field
  integer, optional, intent(in) :: plen
  integer, optional, intent(in) :: to_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_send_i4_0d

 subroutine mpp_send_i8_0d(field,plen,to_pe,block)
  integer(kind=8), intent(inout) :: field
  integer, optional, intent(in) :: plen
  integer, optional, intent(in) :: to_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_send_i8_0d

 subroutine mpp_recv_r4_0d(field,glen,from_pe,block)
  real(kind=4), intent(inout) :: field
  integer, optional, intent(in) :: glen
  integer, optional, intent(in) :: from_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_recv_r4_0d

 subroutine mpp_recv_r8_0d(field,glen,from_pe,block)
  real(kind=8), intent(inout) :: field
  integer, optional, intent(in) :: glen
  integer, optional, intent(in) :: from_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_recv_r8_0d

 subroutine mpp_recv_r4_3d(field,glen,from_pe,block)
  real(kind=4), intent(inout) :: field(:,:,:)
  integer, optional, intent(in) :: glen
  integer, optional, intent(in) :: from_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_recv_r4_3d

 subroutine mpp_recv_r8_3d(field,glen,from_pe,block)
  real(kind=8), intent(inout) :: field(:,:,:)
  integer, optional, intent(in) :: glen
  integer, optional, intent(in) :: from_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_recv_r8_3d

 subroutine mpp_recv_i4_0d(field,glen,from_pe,block)
  integer(kind=4), intent(inout) :: field
  integer, optional, intent(in) :: glen
  integer, optional, intent(in) :: from_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_recv_i4_0d

 subroutine mpp_recv_i8_0d(field,glen,from_pe,block)
  integer(kind=8), intent(inout) :: field
  integer, optional, intent(in) :: glen
  integer, optional, intent(in) :: from_pe
  logical, optional, intent(in) :: block
 endsubroutine mpp_recv_i8_0d

 subroutine mpp_gather_i4_1d(sbuf, rbuf, pelist)
  integer, dimension(:),    intent(in) :: sbuf
  integer, dimension(:), intent(inout) :: rbuf
  integer,   dimension(:),    intent(in), optional :: pelist(:)
 end subroutine mpp_gather_i4_1d

 subroutine mpp_gather_r4_1dv(sbuf, ssize, rbuf, rsize, pelist)
  real(kind=4), dimension(:),    intent(in) :: sbuf
  real(kind=4), dimension(:), intent(inout) :: rbuf
  integer,                    intent(in) :: ssize
  integer,   dimension(:),    intent(in) :: rsize
  integer,   dimension(:),    intent(in), optional :: pelist(:)
 end subroutine mpp_gather_r4_1dv

 subroutine mpp_gather_r8_1dv(sbuf, ssize, rbuf, rsize, pelist)
  real(kind=8), dimension(:),    intent(in) :: sbuf
  real(kind=8), dimension(:), intent(inout) :: rbuf
  integer,                    intent(in) :: ssize
  integer,   dimension(:),    intent(in) :: rsize
  integer,   dimension(:),    intent(in), optional :: pelist(:)
 end subroutine mpp_gather_r8_1dv

 
 subroutine mpp_gather_pelist_r4_2d(is, ie, js, je, pelist, array_seg, data, is_root_pe, &
  ishift, jshift)
integer,                           intent(in)    :: is, ie, js, je
integer,   dimension(:),           intent(in)    :: pelist
real(kind=4), dimension(is:ie,js:je), intent(in)    :: array_seg
real(kind=4), dimension(:,:),         intent(inout) :: data
logical,                           intent(in)    :: is_root_pe
integer,   optional,               intent(in)    :: ishift, jshift
 end subroutine mpp_gather_pelist_r4_2d

 subroutine mpp_gather_pelist_r8_2d(is, ie, js, je, pelist, array_seg, data, is_root_pe, &
  ishift, jshift)
integer,                           intent(in)    :: is, ie, js, je
integer,   dimension(:),           intent(in)    :: pelist
real(kind=8), dimension(is:ie,js:je), intent(in)    :: array_seg
real(kind=8), dimension(:,:),         intent(inout) :: data
logical,                           intent(in)    :: is_root_pe
integer,   optional,               intent(in)    :: ishift, jshift
 end subroutine mpp_gather_pelist_r8_2d

 subroutine mpp_gather_pelist_r4_3d(is, ie, js, je, nz, pelist, array_seg, data, is_root_pe, &
  ishift, jshift)
integer,                           intent(in)    :: is, ie, js, je, nz
integer,   dimension(:),           intent(in)    :: pelist
real(kind=4), dimension(:,:,:), intent(in)    :: array_seg
real(kind=4), dimension(:,:,:),         intent(inout) :: data
logical,                           intent(in)    :: is_root_pe
integer,   optional,               intent(in)    :: ishift, jshift
 end subroutine mpp_gather_pelist_r4_3d

 subroutine mpp_gather_pelist_r8_3d(is, ie, js, je, nz, pelist, array_seg, data, is_root_pe, &
  ishift, jshift)
integer,                           intent(in)    :: is, ie, js, je, nz
integer,   dimension(:),           intent(in)    :: pelist
real(kind=8), dimension(:,:,:), intent(in)    :: array_seg
real(kind=8), dimension(:,:,:),         intent(inout) :: data
logical,                           intent(in)    :: is_root_pe
integer,   optional,               intent(in)    :: ishift, jshift
 end subroutine mpp_gather_pelist_r8_3d

 subroutine mpp_sync
  print*, 'mpp_sync'
 endsubroutine mpp_sync

subroutine mpp_set_warn_level(flag)
  integer, intent(in) :: flag
end subroutine mpp_set_warn_level

subroutine mpp_declare_pelist( pelist, name )
  integer,                    intent(in) :: pelist(:)
  character(len=*), intent(in), optional :: name
end subroutine mpp_declare_pelist

subroutine mpp_set_current_pelist( pelist, no_sync )
  integer, intent(in), optional :: pelist(:)
  logical, intent(in), optional :: no_sync
end subroutine mpp_set_current_pelist

function mpp_chksum_r4_2d( var )
  integer(i8_kind) :: mpp_chksum_r4_2d
  real(kind=4), intent(in) :: var(:,:)       
end function mpp_chksum_r4_2d

function mpp_chksum_r4_3d( var )
  integer(i8_kind) :: mpp_chksum_r4_3d
  real(kind=4), intent(in) :: var(:,:,:)       
end function mpp_chksum_r4_3d

function mpp_chksum_r4_4d( var )
  integer(i8_kind) :: mpp_chksum_r4_4d
  real(kind=4), intent(in) :: var(:,:,:,:)       
end function mpp_chksum_r4_4d

function mpp_chksum_r8_2d( var )
  integer(i8_kind) :: mpp_chksum_r8_2d
  real(kind=8), intent(in) :: var(:,:)       
end function mpp_chksum_r8_2d

function mpp_chksum_r8_3d( var )
  integer(i8_kind) :: mpp_chksum_r8_3d
  real(kind=8), intent(in) :: var(:,:,:)       
end function mpp_chksum_r8_3d

function mpp_chksum_r8_4d( var )
  integer(i8_kind) :: mpp_chksum_r8_4d
  real(kind=8), intent(in) :: var(:,:,:,:)       
end function mpp_chksum_r8_4d

end module mpp_mod
