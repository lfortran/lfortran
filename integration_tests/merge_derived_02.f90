! MERGE of derived-type values inside an ELEMENTAL function whose mask
! comes from a user-defined elemental operator(>) (issue #15942)
module merge_derived_02_mod
implicit none
type :: time_of_day
integer :: hour,minute,second
end type
interface operator(>)
module procedure time_gt
end interface
contains
elemental logical function time_gt(t1,t2)
type(time_of_day),intent(in)::t1,t2
time_gt=3600*t1%hour+60*t1%minute+t1%second > &
        3600*t2%hour+60*t2%minute+t2%second
end function
elemental function min_time(t1,t2) result(tmin)
type(time_of_day),intent(in)::t1,t2
type(time_of_day)::tmin
tmin=merge(t1,t2,t2>t1)
end function
end module

program merge_derived_02
use merge_derived_02_mod
implicit none
type(time_of_day)::a,b,c
type(time_of_day)::x(2),y(2),z(2)
a=time_of_day(9,0,0)
b=time_of_day(3,30,0)
c=min_time(a,b)
print *,c%hour,c%minute,c%second
if (c%hour /= 3 .or. c%minute /= 30 .or. c%second /= 0) error stop 1
c=min_time(b,a)
if (c%hour /= 3 .or. c%minute /= 30 .or. c%second /= 0) error stop 2
x=[time_of_day(1,2,3), time_of_day(10,0,0)]
y=[time_of_day(4,5,6), time_of_day(9,59,59)]
z=min_time(x,y)
print *,z%hour
if (any(z%hour /= [1,9])) error stop 3
if (any(z%minute /= [2,59])) error stop 4
if (any(z%second /= [3,59])) error stop 5
end program
