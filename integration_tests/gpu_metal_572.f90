module gpu_metal_572_m
implicit none
contains

! The contained procedure reads a named constant of its host.
pure real function f(k)
    integer, intent(in) :: k
    integer, parameter :: hp = 2
    f = g() + k
contains
    pure real function g()
        g = hp
    end function
end function

! The contained procedure reads a local variable of its host, also through
! another procedure of the host.
pure real function h(k)
    integer, intent(in) :: k
    real :: s
    s = k
    h = g()
contains
    pure real function g()
        g = s + e()
    end function
    pure real function e()
        e = 2*s
    end function
end function

! The contained procedure reads a local array and an array constant of
! its host.
pure integer function u(k)
    integer, intent(in) :: k
    integer, parameter :: c(3) = [10, 20, 30]
    integer :: a(3)
    a = [1, 2, 3] * k
    u = w(2)
contains
    pure integer function w(j)
        integer, intent(in) :: j
        w = sum(a) + c(j)
    end function
end function
end module

program gpu_metal_572
use gpu_metal_572_m, only: f, h, u
implicit none
integer, parameter :: n = 10000
real :: o(n), p(n)
integer :: q(n), i
do concurrent (i = 1:n)
    o(i) = f(i)
    p(i) = h(i)
    q(i) = u(i)
end do
print *, o(1:4)
print *, p(1:4)
print *, q(1:4)
do i = 1, n
    if (o(i) /= i + 2) error stop 1
    if (p(i) /= 3*i) error stop 2
    if (q(i) /= 6*i + 20) error stop 3
end do
end program
