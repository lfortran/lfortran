program loop_fusion_02
    ! With --fast, the two nests become one, the index `ia` of the second
    ! one becomes `k` and the statement in between runs after the nest.
    implicit none
    integer, parameter :: n = 5
    real :: a(n,n), b(n,n), s
    integer :: i, j, k, l, ia
    do j = 1, n
        do i = 1, n
            a(i,j) = i + 10*j
        end do
    end do
    s = 3
    do l = 1, n
        ia = 1
        do k = 1, n
            b(k,l) = a(ia,l) * 2
            ia = ia + 1
        end do
    end do
    if (s /= 3) error stop 1
    if (abs(sum(b) - 2*sum(a)) > 1e-3) error stop 2
    if (abs(b(2,3) - 64) > 1e-6) error stop 3
    print *, "ok"
end program
