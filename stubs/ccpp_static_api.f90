module ccpp_static_api

    use CCPP_data,         only: ccpp_t

    public

    contains

    subroutine ccpp_physics_run(cdata, suite_name, group_name, ierr)

        type(ccpp_t), intent(inout) :: cdata
        character(len=*), intent(in) :: suite_name, group_name
        integer, intent(out) :: ierr

    end subroutine ccpp_physics_run

    subroutine ccpp_physics_timestep_init(cdata, suite_name, group_name, ierr)

        type(ccpp_t), intent(inout) :: cdata
        character(len=*), intent(in) :: suite_name, group_name
        integer, intent(out) :: ierr

    end subroutine ccpp_physics_timestep_init

    subroutine ccpp_physics_timestep_finalize(cdata, suite_name, group_name, ierr)

        type(ccpp_t), intent(inout) :: cdata
        character(len=*), intent(in) :: suite_name, group_name
        integer, intent(out) :: ierr

    end subroutine ccpp_physics_timestep_finalize

end module ccpp_static_api
