program lapack_testing_02
    implicit none

    real, allocatable :: work(:)

    allocate(work(10))
    work = 0.0

    call lapack_testing_02_foo(work)

    if (abs(work(1) - 2.0) > 1e-6) error stop
end program lapack_testing_02
