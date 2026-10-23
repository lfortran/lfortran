! gpu decline reason: LiveoutScalar
! An allocatable scalar the loop defines, whose value the host reads after
! it, which the buffer that carries a value back to the host cannot hold:
! whether the scalar is allocated after the loop depends on the
! iterations.
program gpu_decline_28
    implicit none
    integer, allocatable :: k
    integer :: i, a(100)
    do concurrent (i = 1:100)
        a(i) = i
        if (i == 5) k = i
    end do
    print *, allocated(k), k
    if (k /= 5) error stop 1
    if (a(100) /= 100) error stop 2
end program
