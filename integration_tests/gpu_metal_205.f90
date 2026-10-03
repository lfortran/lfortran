program gpu_metal_205
! A recursive routine called from a do concurrent loop. Metal Shading
! Language has no call stack of its own, but the shader compiler accepts a
! recursive device routine of a small, bounded depth, so the loop is
! offloaded and the routine is emitted with the rest of the device code.
implicit none
integer :: i
integer :: a(5)

a = 0
do concurrent (i = 1:5)
    a(i) = fact(i)
end do

if (a(1) /= 1) error stop
if (a(5) /= 120) error stop
print *, "PASSED"
contains
    pure recursive function fact(n) result(r)
        integer, intent(in) :: n
        integer :: r
        if (n <= 1) then
            r = 1
        else
            r = n * fact(n - 1)
        end if
    end function
end program
