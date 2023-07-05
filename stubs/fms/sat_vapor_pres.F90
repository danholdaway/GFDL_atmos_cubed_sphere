module sat_vapor_pres_mod
    implicit none

    integer, public :: tcmin = -160  ! minimum temperature (degC) in lookup table
    integer, public :: tcmax =  100  ! maximum temperature (degC) in lookup table
    
contains

    subroutine compute_qs(t, pfull, rh, q, es_over_liq_and_ice)
        real, intent (in),  dimension(:,:) :: pfull, t
        real, intent (out), dimension(:,:) :: rh
        real, intent (in),  dimension(:,:) :: q
        logical, optional, intent(in) :: es_over_liq_and_ice
    end subroutine compute_qs

    subroutine lookup_es(t,esat)
        real, intent(in),  dimension(:,:) :: t
        real, intent(inout), dimension(:,:) :: esat
    end subroutine lookup_es
    
end module sat_vapor_pres_mod