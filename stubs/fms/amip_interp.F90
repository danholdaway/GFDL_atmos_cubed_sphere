module amip_interp_mod

    integer :: i_sst = 1200
    integer :: j_sst = 600
    logical :: forecast_mode = .false.
    real, allocatable, dimension(:,:) ::  sst_ncep, sst_anom

    logical :: use_ncep_sst = .false. !< SJL: During nudging:   use_ncep_sst = .T.;  no_anom_sst = .T.
                                      !!      during forecast:  use_ncep_sst = .T.;  no_anom_sst = .F.

    public i_sst, j_sst, sst_ncep, sst_anom, forecast_mode, use_ncep_sst
contains
    
end module amip_interp_mod