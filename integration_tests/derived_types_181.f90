module derived_types_181_mod
implicit none

type :: pt
    real :: x, y
end type pt

contains

    subroutine scal_no_intent(v, n)
    integer :: n
    real :: v(n)
    v = 2 * v
    end subroutine scal_no_intent

    subroutine scal_inout(v, n)
    integer, intent(in) :: n
    real, intent(inout) :: v(n)
    v = 2 * v
    end subroutine scal_inout

    subroutine set_out(v, n)
    integer, intent(in) :: n
    real, intent(out) :: v(n)
    integer :: i
    do i = 1, n
        v(i) = real(10 * i)
    end do
    end subroutine set_out

    subroutine sum_in(v, n, s)
    integer, intent(in) :: n
    real, intent(in) :: v(n)
    real, intent(out) :: s
    s = sum(v)
    end subroutine sum_in

    subroutine scal_assumed_shape(v)
    real, intent(inout) :: v(:)
    v = 2 * v
    end subroutine scal_assumed_shape

    subroutine scal_plain(v, n)
    integer, intent(in) :: n
    real, intent(inout) :: v(n)
    v = 2 * v
    end subroutine scal_plain

end module derived_types_181_mod

program derived_types_181
use derived_types_181_mod
implicit none

type(pt) :: ps(4)
real :: a(4), s
integer :: i

! a component of an array of derived type, dummy without a declared intent
ps%x = 1.0
ps%y = 1.0
call scal_no_intent(ps%x, 4)
if (any(ps%x /= 2.0)) error stop "no intent: copy-out lost"
if (any(ps%y /= 1.0)) error stop "no intent: neighbouring component clobbered"

! intent(inout)
call scal_inout(ps%y, 4)
if (any(ps%x /= 2.0)) error stop "intent(inout): neighbouring component clobbered"
if (any(ps%y /= 2.0)) error stop "intent(inout): copy-out lost"

! intent(out)
call set_out(ps%x, 4)
do i = 1, 4
    if (ps(i)%x /= real(10 * i)) error stop "intent(out): copy-out lost"
end do
if (any(ps%y /= 2.0)) error stop "intent(out): neighbouring component clobbered"

! intent(in): the component is read, nothing is written back
ps%x = [1.0, 2.0, 3.0, 4.0]
ps%y = [5.0, 6.0, 7.0, 8.0]
call sum_in(ps%x, 4, s)
if (s /= 10.0) error stop "intent(in): wrong values read"
if (any(ps%x /= [1.0, 2.0, 3.0, 4.0])) error stop "intent(in): component modified"

! a strided section of the array of derived type
call scal_no_intent(ps(1::2)%y, 2)
if (any(ps%y /= [10.0, 6.0, 14.0, 8.0])) error stop "strided section: wrong result"

! an assumed-shape dummy
ps%x = [1.0, 2.0, 3.0, 4.0]
ps%y = [5.0, 6.0, 7.0, 8.0]
call scal_assumed_shape(ps%x)
if (any(ps%x /= [2.0, 4.0, 6.0, 8.0])) error stop "assumed shape: copy-out lost"
if (any(ps%y /= [5.0, 6.0, 7.0, 8.0])) error stop "assumed shape: neighbouring component clobbered"

! reading and assigning a component of a strided section directly
ps%y = [1.0, 2.0, 3.0, 4.0]
a(1:2) = ps(1::2)%y
if (any(a(1:2) /= [1.0, 3.0])) error stop "strided section: wrong values read"
ps(1::2)%y = [7.0, 8.0]
if (any(ps%y /= [7.0, 2.0, 8.0, 4.0])) error stop "strided section: wrong assignment"

! a plain contiguous array argument keeps working
a = [1.0, 2.0, 3.0, 4.0]
call scal_plain(a, 4)
if (any(a /= [2.0, 4.0, 6.0, 8.0])) error stop "contiguous array argument"

print *, ps%x
print *, ps%y
print *, a
end program derived_types_181
