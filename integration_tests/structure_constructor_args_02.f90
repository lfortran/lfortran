module structure_constructor_args_02_m
implicit none
integer(8), parameter :: base = 5_8
type :: t
    real :: x(3)
    integer(8) :: a(2, 2) = 0
    logical :: l(2) = .false.
    complex :: z(2) = (0.0, 0.0)
end type
type :: u
    type(t) :: part = t(2.0, base, .true., (1.0, 2.0))
end type
type(t) :: g = t(5, base, .true., (3.0, 4.0))
end module

module structure_constructor_args_02_n
use structure_constructor_args_02_m, only: w => t, base
implicit none
type(w) :: mvk = w(4.0, base, .true., (0.0, 1.0))
end module

program structure_constructor_args_02
use structure_constructor_args_02_m
use structure_constructor_args_02_n, only: mvk
implicit none
type(t) :: lv = t(6.0, 7_8, .true., (1.0, -1.0))
type(t) :: a
type(u) :: v
real :: r
r = 8.0
call check(g, 5.0, base, (3.0, 4.0))
call check(mvk, 4.0, base, (0.0, 1.0))
call check(lv, 6.0, 7_8, (1.0, -1.0))
call check(v%part, 2.0, base, (1.0, 2.0))
a = t(1.5, 3_8, .true., (0.5, 0.5))
call check(a, 1.5, 3_8, (0.5, 0.5))
a = t(r, int(r, 8), .true., cmplx(r, r))
call check(a, 8.0, 8_8, (8.0, 8.0))

contains

subroutine check(s, x, n, z)
    type(t), intent(in) :: s
    real, intent(in) :: x
    integer(8), intent(in) :: n
    complex, intent(in) :: z
    print *, s%x, s%a, s%l, s%z
    if (any(s%x /= x)) error stop 1
    if (any(s%a /= n)) error stop 2
    if (.not. all(s%l)) error stop 3
    if (any(s%z /= z)) error stop 4
end subroutine

end program
