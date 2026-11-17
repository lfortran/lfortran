! unpack with a scalar derived-type FIELD in a named constant initializer
! is folded at compile time (#17878)
program unpack_derived_type_param_01
implicit none
type t
    integer :: i
end type
type t2
    integer :: i
    real :: r
end type
logical, parameter :: m(3) = [.true., .false., .true.]
type(t), parameter :: a(2) = [t(3), t(4)]
type(t), parameter :: u(3) = unpack(a, m, t(0))
type(t2), parameter :: b(2) = [t2(3, 1.5), t2(4, 2.5)]
type(t2), parameter :: v(3) = unpack(b, m, t2(0, -1.0))
print *, u%i
print *, v%i
print *, v%r
if (any(u%i /= [3, 0, 4])) error stop
if (any(v%i /= [3, 0, 4])) error stop
if (any(v%r /= [1.5, -1.0, 2.5])) error stop
end program
