! A `do concurrent` with `reduce` clauses inside a BLOCK nested in an outer
! `do concurrent` stays a sequential loop of the outer loop's body; its
! reductions must keep referring to the BLOCK's variables when the BLOCK is
! copied into the kernel or the outlined parallel region.
program gpu_metal_626
    implicit none
    integer, parameter :: n = 6, m = 5
    real :: a(m,n), r(n), rmax(n), rmin(n)
    integer :: cnt(n)
    integer :: i, j

    do i = 1, n
        do j = 1, m
            a(j,i) = real(mod(i*7 + j*3, 11)) - 4.0
        end do
    end do

    ! BLOCK-local reduction variables
    do concurrent (i = 1:n)
        block
            real :: bacc, bmx, bmn
            integer :: bcnt
            bacc = 0
            bmx = a(1,i)
            bmn = a(1,i)
            bcnt = 0
            do concurrent (j = 1:m) reduce(+:bacc, bcnt) reduce(max:bmx) &
                    reduce(min:bmn)
                bacc = bacc + a(j,i)
                bmx = max(bmx, a(j,i))
                bmn = min(bmn, a(j,i))
                if (a(j,i) > 0) bcnt = bcnt + 1
            end do
            r(i) = bacc
            rmax(i) = bmx
            rmin(i) = bmn
            cnt(i) = bcnt
        end block
    end do
    do i = 1, n
        if (abs(r(i) - sum(a(:,i))) > 1e-4) error stop 1
        if (abs(rmax(i) - maxval(a(:,i))) > 1e-6) error stop 2
        if (abs(rmin(i) - minval(a(:,i))) > 1e-6) error stop 3
        if (cnt(i) /= count(a(:,i) > 0)) error stop 4
    end do
    print *, "ok"
end program
