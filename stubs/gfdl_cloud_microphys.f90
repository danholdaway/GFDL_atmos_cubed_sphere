module gfdl_cloud_microphys_mod

real, public :: wqs1 = 0.0
real, public :: wqs2(1,2,3) = 0.0
real, public :: wqsat2_moist(1,2,3,4) = 0.0
real, public :: wqsat_moist(1,2,3) = 0.0
real, public :: qs_blend = 0.0

contains

subroutine qsmith_init()
end subroutine qsmith_init

end module gfdl_cloud_microphys_mod



