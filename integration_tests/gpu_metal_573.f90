module gpu_metal_573_m
implicit none
contains

pure integer function h(k)
    integer, intent(in) :: k
    h = 2*k + e()
end function

pure integer function e()
    e = 1
end function

pure subroutine s(k, r)
    integer, intent(in) :: k
    integer, intent(out) :: r
    r = 3*k
end subroutine

! The contained function calls a module function, which calls another one.
pure integer function f(k)
    integer, intent(in) :: k
    f = g(k)
contains
    pure integer function g(j)
        integer, intent(in) :: j
        g = h(j)
    end function
end function

! The contained subroutine calls a module subroutine.
pure integer function u(k)
    integer, intent(in) :: k
    integer :: m
    m = k
    call w()
contains
    pure subroutine w()
        call s(m, u)
    end subroutine
end function
end module

program gpu_metal_573
use gpu_metal_573_m, only: f, u
implicit none
integer, parameter :: n = 10000
integer :: o(n), p(n), i
do concurrent (i = 1:n)
    o(i) = f(i)
    p(i) = u(i)
end do
print *, o(1:4)
print *, p(1:4)
do i = 1, n
    if (o(i) /= 2*i + 1) error stop 1
    if (p(i) /= 3*i) error stop 2
end do
end program
