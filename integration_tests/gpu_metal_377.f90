program gpu_metal_377
! Reuse of inner do-loop variable across multiple do concurrent constructs.
! Verifies that inner do-loop variables are treated as thread-local scalars
! rather than liveout buffers when referenced in later loops.
implicit none
integer :: i, j
real :: a(10, 2), b(10, 2)

do concurrent (i = 1:10)
    do j = 1, 2
        a(i, j) = real(i + j)
    end do
end do

do concurrent (i = 1:10)
    do j = 1, 2
        b(i, j) = a(i, j) * 2.0
    end do
end do

do i = 1, 10
    do j = 1, 2
        if (abs(a(i, j) - real(i + j)) > 1.0e-5) error stop 1
        if (abs(b(i, j) - real(i + j) * 2.0) > 1.0e-5) error stop 2
    end do
end do

print *, "PASSED"
end program
