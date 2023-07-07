module CCPP_data

public

type ccpp_t
 character(len=20) :: errmsg
 contains
    procedure :: initialized
end type ccpp_t

type GFDL_interstitial_type
 real, allocatable :: cappa(:,:,:)
 real, allocatable :: te0(:,:,:)
 real, allocatable :: dtdt(:,:,:)
 logical :: last_step
 real, allocatable :: te0_2d(:,:)
 logical :: out_dt
 real :: fast_mp_consv
 real :: kmp
 contains
    procedure :: reset
end type GFDL_interstitial_type

type(ccpp_t), save, target :: cdata_tile

character(len=256)      :: ccpp_suite='undefined'

type(GFDL_interstitial_type), allocatable, save, target :: GFDL_interstitial


contains

logical function initialized(self) result(res)
class(ccpp_t), intent(in) :: self
res = .true.
end function

subroutine reset(self)
    class(GFDL_interstitial_type), intent(inout) :: self
end subroutine reset

end module CCPP_data
