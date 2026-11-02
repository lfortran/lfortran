! A `do concurrent` with `reduce` clauses inside an ASSOCIATE nested in an
! outer `do concurrent` stays a sequential loop of the outer loop's body;
! its reductions must keep referring to the outer loop's local variables
! when the ASSOCIATE is copied into the kernel.
program gpu_metal_629
    implicit none
    integer, parameter :: n = 6, m = 5
    real :: a(m,n), r(n), rmax(n), rmin(n)
    real :: acc, mx, mn
    integer :: cnt(n), ic
    integer :: i, j

    do i = 1, n
        do j = 1, m
            a(j,i) = real(mod(i*7 + j*3, 11)) - 4.0
        end do
    end do

    do concurrent (i = 1:n) local(acc, mx, mn, ic)
        associate (c => i)
            acc = 0
            mx = a(1,c)
            mn = a(1,c)
            ic = 0
            do concurrent (j = 1:m) reduce(+:acc, ic) reduce(max:mx) &
                    reduce(min:mn)
                acc = acc + a(j,c)
                mx = max(mx, a(j,c))
                mn = min(mn, a(j,c))
                if (a(j,c) > 0) ic = ic + 1
            end do
            r(c) = acc
            rmax(c) = mx
            rmin(c) = mn
            cnt(c) = ic
        end associate
    end do
    do i = 1, n
        if (abs(r(i) - sum(a(:,i))) > 1e-4) error stop 1
        if (abs(rmax(i) - maxval(a(:,i))) > 1e-6) error stop 2
        if (abs(rmin(i) - minval(a(:,i))) > 1e-6) error stop 3
        if (cnt(i) /= count(a(:,i) > 0)) error stop 4
    end do
    print *, "ok"
end program
