! An `!$omp target` / `distribute parallel do` loop whose body runs a BLOCK
! with an inner `do concurrent`: the inner loop stays a sequential loop of
! the offloaded loop, so a strided section actual is gathered per thread,
! and its `reduce` clause keeps referring to the BLOCK's variable.
program gpu_metal_627
    implicit none
    integer, parameter :: nh = 5, nd = 3, nk = 4
    integer :: p(nd,nk), h, d, k, j
    real :: s(nd,nh), a(nk,nh), r(nh)

    do k = 1, nk
        do d = 1, nd
            p(d,k) = 10*d + k
        end do
    end do
    do h = 1, nh
        do k = 1, nk
            a(k,h) = real(h*k)
        end do
    end do

    !$omp target map(tofrom: s, p)
    !$omp teams
    !$omp distribute parallel do
    do h = 1, nh
        block
            do concurrent (d = 1:nd)
                s(d,h) = total(p(d,:)) + real(h)
            end do
        end block
    end do
    !$omp end distribute parallel do
    !$omp end teams
    !$omp end target

    !$omp target map(tofrom: a, r)
    !$omp teams
    !$omp distribute parallel do
    do h = 1, nh
        block
            real :: acc
            acc = 0
            do concurrent (j = 1:nk) reduce(+:acc)
                acc = acc + a(j,h)
            end do
            r(h) = acc
        end block
    end do
    !$omp end distribute parallel do
    !$omp end teams
    !$omp end target

    do h = 1, nh
        do d = 1, nd
            if (abs(s(d,h) - (real(sum(p(d,:))) + real(h))) > 1e-5) error stop 1
        end do
        if (abs(r(h) - sum(a(:,h))) > 1e-4) error stop 2
    end do
    print *, "ok"

contains

    pure real function total(b)
        integer, intent(in) :: b(:)
        total = real(sum(b))
    end function

end program
