module gpu_metal_574_m
implicit none
contains

pure integer function h(k)
    integer, intent(in) :: k
    h = 2*k
end function

! The contained function passes an intent(in) dummy of its host on.
pure integer function f(i)
    integer, intent(in) :: i
    f = g()
contains
    pure integer function g()
        g = h(i)
    end function
end function

! The contained subroutine reads an intent(in) dummy of its host, also
! through another procedure of the host.
pure integer function u(k)
    integer, intent(in) :: k
    call w()
contains
    pure subroutine w()
        u = 3*k + v()
    end subroutine
    pure integer function v()
        v = k
    end function
end function
end module

program gpu_metal_574
use gpu_metal_574_m, only: f, u
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
    if (o(i) /= 2*i) error stop 1
    if (p(i) /= 4*i) error stop 2
end do
end program
