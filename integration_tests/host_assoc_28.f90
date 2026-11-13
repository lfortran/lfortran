! Internal procedures of a bind(c) procedure use its value dummy arguments
module host_assoc_28_m
use iso_c_binding
implicit none
contains

subroutine read_value(n, d, flag, x, y, z) bind(c, name="host_assoc_28_read")
    integer(c_int), intent(in), value :: n
    real(c_double), intent(in), value :: d
    logical(c_bool), intent(in), value :: flag
    integer(c_int), intent(out) :: x
    real(c_double), intent(out) :: y
    logical(c_bool), intent(out) :: z
    call inner()
contains
    subroutine inner()
        x = n + 1
        y = d * 2
        z = .not. flag
    end subroutine inner
end subroutine read_value

! A value dummy is a local copy: its modifications are shared by the host and
! its internal procedures, and not seen by the caller
subroutine modify_value(n, d, x, y) bind(c, name="host_assoc_28_modify")
    integer(c_int), value :: n
    real(c_double), value :: d
    integer(c_int), intent(out) :: x
    real(c_double), intent(out) :: y
    n = n + 10
    call bump()
    x = n
    d = d + 0.5_c_double
    y = d
contains
    subroutine bump()
        n = n * 2
        d = d + 1
    end subroutine bump
end subroutine modify_value

subroutine sized(n, a, s) bind(c, name="host_assoc_28_sized")
    integer(c_int), value :: n
    integer(c_int), intent(in) :: a(n)
    integer(c_int), intent(out) :: s
    s = 0
    call total()
contains
    subroutine total()
        integer :: i
        do i = 1, n
            s = s + a(i)
        end do
    end subroutine total
end subroutine sized

! A value dummy is passed on by value and by reference
integer(c_int) function twice(i) bind(c, name="host_assoc_28_twice")
    integer(c_int), value :: i
    twice = 2 * i
end function twice

integer(c_int) function plus_one(i) bind(c, name="host_assoc_28_plus_one")
    integer(c_int), intent(in) :: i
    plus_one = i + 1
end function plus_one

subroutine forward(n, r) bind(c, name="host_assoc_28_forward")
    integer(c_int), value :: n
    integer(c_int), intent(out) :: r
    r = twice(n) + plus_one(n)
    call inner()
contains
    subroutine inner()
        r = r + twice(n)
    end subroutine inner
end subroutine forward

end module host_assoc_28_m

program host_assoc_28
use iso_c_binding
use host_assoc_28_m
implicit none
interface
    subroutine c_read(n, d, flag, x, y, z) bind(c, name="host_assoc_28_read")
        import :: c_int, c_double, c_bool
        integer(c_int), intent(in), value :: n
        real(c_double), intent(in), value :: d
        logical(c_bool), intent(in), value :: flag
        integer(c_int), intent(out) :: x
        real(c_double), intent(out) :: y
        logical(c_bool), intent(out) :: z
    end subroutine c_read
end interface
integer(c_int) :: x, n
real(c_double) :: y, d
logical(c_bool) :: z

call read_value(3_c_int, 1.25_c_double, .true._c_bool, x, y, z)
if (x /= 4) error stop 1
if (abs(y - 2.5_c_double) > 1e-12_c_double) error stop 2
if (z) error stop 3

call c_read(7_c_int, -1.5_c_double, .false._c_bool, x, y, z)
if (x /= 8) error stop 4
if (abs(y + 3.0_c_double) > 1e-12_c_double) error stop 5
if (.not. z) error stop 6

n = 1
d = 1.0_c_double
call modify_value(n, d, x, y)
if (x /= 22) error stop 7
if (abs(y - 2.5_c_double) > 1e-12_c_double) error stop 8
if (n /= 1) error stop 9
if (abs(d - 1.0_c_double) > 1e-12_c_double) error stop 10

call sized(3_c_int, [1_c_int, 2_c_int, 3_c_int, 4_c_int], x)
if (x /= 6) error stop 11
call forward(5_c_int, x)
if (x /= 26) error stop 12
print *, "ok"
end program host_assoc_28
