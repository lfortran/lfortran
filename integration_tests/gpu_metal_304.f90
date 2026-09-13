! Scalar ASSOCIATE wrapping an array ASSOCIATE inside a BLOCK of a
! do concurrent. The inner ASSOCIATE is rebuilt as a BLOCK; that BLOCK
! must live in the enclosing BLOCK's table so a successful offload does
! not leave a BlockCall into the host procedure.
program gpu_metal_304
implicit none
integer, parameter :: n = 4
real :: x(n), a(n)
integer :: i

x = [1.0, 2.0, 3.0, 4.0]
a = 0.0

do concurrent (i = 1:n)
    block
        associate (t => i)
            associate (r => x + real(t))
                a(i) = r(i)
            end associate
        end associate
    end block
end do

do i = 1, n
    if (abs(a(i) - (x(i) + real(i))) > 1.0e-5) error stop "nested assoc"
end do

print *, "PASS"
end program
