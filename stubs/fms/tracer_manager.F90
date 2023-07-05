module tracer_manager_mod

implicit none
private

public get_tracer_index, get_tracer_names, get_number_tracers

contains

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

