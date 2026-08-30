program gpu_metal_239
! `do concurrent` nests of rank 4 and 5 must be offloaded to the GPU, the same
! way rank 1, 2 and 3 nests already are.  The dispatch grid is a flat 1-D grid
! of product(extents) threads and each index is recovered by successive divmod,
! so the rank is not limited by the 3-D shape of the dispatch grid.
!
! Unequal extents and non-unit lower bounds are used on purpose: a wrong divmod
! decomposition then produces wrong VALUES, not merely a wrong shape.
!
! Under `--gpu=metal` this file must emit six kernels, one per subroutine
! (rank1, rank2, rank3, rank4, rank4_shifted, rank5); before the rank cap was
! lifted only the first three were emitted and the rank 4 and 5 nests were
! silently left on the CPU.
implicit none

call rank1()
call rank2()
call rank3()
call rank4()
call rank4_shifted()
call rank5()

contains

    subroutine rank1()
        real :: a(7)
        integer :: i
        a = 0.0
        do concurrent (i=1:7)
            a(i) = real(i)
        end do
        do i = 1, 7
            if (abs(a(i) - real(i)) > 1.0e-5) error stop "rank1"
        end do
        print *, "rank1", a(7)
    end subroutine

    subroutine rank2()
        real :: a(3,5)
        integer :: i, j
        a = 0.0
        do concurrent (i=1:3, j=1:5)
            a(i,j) = real(i) + 10.0*real(j)
        end do
        do j = 1, 5
            do i = 1, 3
                if (abs(a(i,j) - (real(i) + 10.0*real(j))) > 1.0e-5) error stop "rank2"
            end do
        end do
        print *, "rank2", a(3,5)
    end subroutine

    subroutine rank3()
        real :: a(2,3,4)
        integer :: i, j, k
        a = 0.0
        do concurrent (i=1:2, j=1:3, k=1:4)
            a(i,j,k) = real(i) + 10.0*real(j) + 100.0*real(k)
        end do
        do k = 1, 4
            do j = 1, 3
                do i = 1, 2
                    if (abs(a(i,j,k) - (real(i) + 10.0*real(j) + 100.0*real(k))) &
                        > 1.0e-5) error stop "rank3"
                end do
            end do
        end do
        print *, "rank3", a(2,3,4)
    end subroutine

    subroutine rank4()
        real :: a(2,3,4,5)
        integer :: i, j, k, m
        a = 0.0
        do concurrent (i=1:2, j=1:3, k=1:4, m=1:5)
            a(i,j,k,m) = real(i) + 10.0*real(j) + 100.0*real(k) + 1000.0*real(m)
        end do
        do m = 1, 5
            do k = 1, 4
                do j = 1, 3
                    do i = 1, 2
                        if (abs(a(i,j,k,m) - (real(i) + 10.0*real(j) &
                            + 100.0*real(k) + 1000.0*real(m))) > 1.0e-5) then
                            error stop "rank4"
                        end if
                    end do
                end do
            end do
        end do
        print *, "rank4", a(2,3,4,5)
    end subroutine

    subroutine rank4_shifted()
        ! Non-unit lower bounds with unequal extents.
        real :: a(0:1, 4:6, 3:6, 2:4)
        integer :: i, j, k, m
        a = 0.0
        do concurrent (i=0:1, j=4:6, k=3:6, m=2:4)
            a(i,j,k,m) = real(i) + 10.0*real(j) + 100.0*real(k) + 1000.0*real(m)
        end do
        do m = 2, 4
            do k = 3, 6
                do j = 4, 6
                    do i = 0, 1
                        if (abs(a(i,j,k,m) - (real(i) + 10.0*real(j) &
                            + 100.0*real(k) + 1000.0*real(m))) > 1.0e-5) then
                            error stop "rank4_shifted"
                        end if
                    end do
                end do
            end do
        end do
        print *, "rank4_shifted", a(1,6,6,4)
    end subroutine

    subroutine rank5()
        real :: a(2,3,2,4,3)
        integer :: i, j, k, m, p
        a = 0.0
        do concurrent (i=1:2, j=1:3, k=1:2, m=1:4, p=1:3)
            a(i,j,k,m,p) = real(i) + 10.0*real(j) + 100.0*real(k) &
                + 1000.0*real(m) + 10000.0*real(p)
        end do
        do p = 1, 3
            do m = 1, 4
                do k = 1, 2
                    do j = 1, 3
                        do i = 1, 2
                            if (abs(a(i,j,k,m,p) - (real(i) + 10.0*real(j) &
                                + 100.0*real(k) + 1000.0*real(m) &
                                + 10000.0*real(p))) > 1.0e-5) then
                                error stop "rank5"
                            end if
                        end do
                    end do
                end do
            end do
        end do
        print *, "rank5", a(2,3,2,4,3)
    end subroutine

end program
