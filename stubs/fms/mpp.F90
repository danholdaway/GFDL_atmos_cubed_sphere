module mpp_mod

  ! MPP_DEBUG, NOTE, MPP_CLOCK_SYNC,MPP_CLOCK_DETAILED, WARNING

use platform_mod, only: r8_kind, i8_kind
use mpp_parameter_mod, only: MPP_DEBUG, NOTE, MPP_CLOCK_SYNC,MPP_CLOCK_DETAILED, FATAL, WARNING, EVENT_RECV

implicit none
private

public mpp_error, FATAL, mpp_sum, mpp_sync, mpp_npes, mpp_broadcast, WARNING, mpp_pe, &
       mpp_send, mpp_recv, mpp_sync_self, mpp_max, mpp_root_pe, mpp_clock_begin, &
       mpp_clock_end, mpp_clock_id, lowercase, stdlog, MPP_DEBUG, NOTE, &
       MPP_CLOCK_SYNC,MPP_CLOCK_DETAILED, mpp_set_warn_level, mpp_declare_pelist, &
       mpp_set_current_pelist, mpp_chksum, stdout, stderr, EVENT_RECV, mpp_gather, &
       mpp_get_current_pelist

integer, parameter :: NAME_LENGTH = 64
integer :: mpp_max !dummy
integer, parameter, public :: CLOCK_SUBCOMPONENT=11

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
    module procedure mpp_gather_pelist_r4_2d
    module procedure mpp_gather_pelist_r8_2d
end interface

contains

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

 subroutine mpp_error(fatal,statement)

  integer, intent(in) :: fatal
  character(len=*), intent(in) :: statement

   if (fatal > 0) then
      print*, statement
   endif

 endsubroutine mpp_error

 subroutine mpp_sum(tilelist,npes)
  integer, intent(in) :: tilelist(:), npes
 endsubroutine mpp_sum

 subroutine mpp_broadcast(field,fsize,n,pelist)
  real(r8_kind), intent(inout) :: field(:,:,:)
  integer, intent(in) :: fsize,n,pelist(:)
 endsubroutine mpp_broadcast

 subroutine mpp_send(field,fieldsize,recv_proc)
  real(r8_kind), intent(inout) :: field(:,:,:)
  integer, intent(in) :: fieldsize, recv_proc
 endsubroutine mpp_send

 subroutine mpp_recv(field,fieldsize,send_proc)
  real(r8_kind), intent(inout) :: field(:,:,:)
  integer, intent(in) :: fieldsize, send_proc
 endsubroutine mpp_recv

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


 subroutine mpp_sync
  print*, 'mpp_sync'
 endsubroutine mpp_sync

 subroutine mpp_sync_self
  print*, 'mpp_sync_self'
 endsubroutine mpp_sync_self

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
