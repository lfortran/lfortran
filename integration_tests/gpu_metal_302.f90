! An array-valued ASSOCIATE in a do concurrent body is rebuilt as a
! BLOCK after copy_loop_stmt. That BLOCK must be a kernel copy: a
! later decline must drop it, and the host ASSOCIATE must still run.
program gpu_metal_302
implicit none
integer, parameter :: n = 4
real :: x(n), a(n)
integer :: i

x = [1.0, 2.0, 3.0, 4.0]
a = 0.0

do concurrent (i = 1:n)
    associate (r => x + real(i))
        a(i) = r(i)
    end associate
    if (i < 0) error stop "unreachable"
end do

do i = 1, n
    if (abs(a(i) - (x(i) + real(i))) > 1.0e-5) error stop "associate"
end do

print *, "PASS"
end program
