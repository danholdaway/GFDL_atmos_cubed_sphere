
module time_manager_mod

implicit none
private

public time_type, get_time, month_name, get_date, time_type_to_real
public time_minus, time_divide

! public operator(+), operator(-), operator(/), assignment(=)

type time_type
   private
   integer:: seconds
   integer:: days
   integer:: ticks
   integer:: dummy ! added as a workaround bug on IRIX64 (AP)
end type time_type

! interface operator (+);   module procedure time_plus;        end interface
! interface operator (-);   module procedure time_minus;       end interface
! interface operator (/);   module procedure time_divide;      end interface
! interface assignment (=);   module procedure time_assignment;      end interface

contains

function time_plus(time1, time2)
   type(time_type) :: time_plus
   type(time_type), intent(in) :: time1, time2   
end function time_plus

function time_minus(time1, time2)
   type(time_type) :: time_minus
   type(time_type), intent(in) :: time1, time2   
end function time_minus

function time_divide(time1, time2)
   integer :: time_divide
   type(time_type), intent(in) :: time1, time2   
end function time_divide

subroutine time_assignment(time1, time2)
   type(time_type), intent(out) :: time1
   type(time_type), intent(in)  :: time2
end subroutine time_assignment

real function time_type_to_real(time)
   type(time_type), intent(in) :: time
end function time_type_to_real

function month_name(mn)
   integer, intent(in) :: mn
   character (len=9) :: month_name
end function month_name

subroutine get_date(Time, yr, mon, dd, hr, mn, seconds)
   type(time_type), intent(in) :: Time
   integer, intent(out) :: yr, mon, dd, hr, mn, seconds
end subroutine get_date

subroutine get_time(Time, seconds, days, ticks, err_msg)
   
   type(time_type), intent(in) :: Time
   integer, intent(out) :: seconds
   integer, intent(out), optional :: days, ticks
   character(len=*), intent(out), optional :: err_msg
   character(len=128) :: err_msg_local
   
end subroutine get_time

end module time_manager_mod

