module time_manager_mod

implicit none
private

public time_type, get_time

type time_type
   private
   integer:: seconds
   integer:: days
   integer:: ticks
   integer:: dummy ! added as a workaround bug on IRIX64 (AP)
end type time_type

contains

subroutine get_time(Time, seconds, days, ticks, err_msg)
   
   type(time_type), intent(in) :: Time
   integer, intent(out) :: seconds
   integer, intent(out), optional :: days, ticks
   character(len=*), intent(out), optional :: err_msg
   character(len=128) :: err_msg_local
   
end subroutine get_time

end module time_manager_mod

