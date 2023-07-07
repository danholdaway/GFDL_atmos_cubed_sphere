module tracer_manager_mod
use field_manager_mod
implicit none
private

public get_tracer_index, get_tracer_names, get_number_tracers, set_tracer_profile, &
       adjust_mass, get_tracer_indices, check_if_prognostic, register_tracers

contains

subroutine register_tracers(model, num_tracers, num_prog, num_diag, num_family)
    integer, intent(in) :: model 
    integer, intent(out) :: num_tracers 
    integer, intent(out) :: num_prog 
    integer, intent(out) :: num_diag 
    integer, intent(out), optional :: num_family
end subroutine

logical function check_if_prognostic(model, n, err_msg)
    integer, intent(in) :: model
    integer, intent(in) :: n 
    character(len=*), intent(out), optional :: err_msg
end function check_if_prognostic

subroutine get_tracer_indices(model, ind, prog_ind, diag_ind, fam_ind)
    integer, intent(in) :: model
    integer, intent(out), dimension(:), optional :: ind, prog_ind, diag_ind, fam_ind
end subroutine get_tracer_indices

logical function adjust_mass(model, n, err_msg)
    integer, intent(in) :: model, n
    character(len=*), intent(out), optional :: err_msg
end function adjust_mass

subroutine set_tracer_profile(model, num_tracers, q)
    integer,  intent(in) :: model 
    integer, intent(in) :: num_tracers 
    real, intent(inout) :: q(:,:,:)
end subroutine

subroutine get_number_tracers(model, num_tracers, num_prog, num_diag, num_family)
    integer,  intent(in) :: model 
    integer, intent(out), optional :: num_tracers 
    integer, intent(out), optional :: num_prog 
    integer, intent(out), optional :: num_diag 
    integer, intent(out), optional :: num_family
end subroutine

subroutine get_tracer_names(model,n,name,longname, units, err_msg)
    integer,          intent(in)  :: model
    integer,          intent(in)  :: n
    character (len=*),intent(out) :: name 
    character (len=*), intent(out), optional :: longname 
    character (len=*), intent(out), optional :: units 
    character (len=*), intent(out), optional :: err_msg
end subroutine

 integer function get_tracer_index (MODEL_ATMOS, TrName) 

  integer, intent(in) :: MODEL_ATMOS
  character(len=*) :: TrName

  get_tracer_index = MODEL_ATMOS

 end function get_tracer_index

end module tracer_manager_mod

