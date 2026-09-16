program openmp_83
    implicit none
    integer :: i, j, s, m, n
    integer :: a(2), b(1000), c(10, 20), d(6, 7), e(50)

    a = 1
    !$omp teams
    !$omp distribute parallel do
    do i = 1, 2
        a(i) = 2
    end do
    !$omp end distribute parallel do
    !$omp end teams
    if (a(1) /= 2 .or. a(2) /= 2) error stop

    b = 0
    !$omp teams num_teams(4)
    !$omp distribute parallel do
    do i = 1, 1000
        b(i) = 2*i
    end do
    !$omp end distribute parallel do
    !$omp end teams
    do i = 1, 1000
        if (b(i) /= 2*i) error stop
    end do

    c = 0
    !$omp teams num_teams(2)
    !$omp distribute parallel do private(j)
    do i = 1, 10
        do j = 1, 20
            c(i, j) = 100*i + j
        end do
    end do
    !$omp end distribute parallel do
    !$omp end teams
    do j = 1, 20
        do i = 1, 10
            if (c(i, j) /= 100*i + j) error stop
        end do
    end do

    d = 0
    !$omp teams num_teams(3)
    !$omp distribute parallel do collapse(2)
    do i = 1, 6
        do j = 1, 7
            d(i, j) = 10*i + j
        end do
    end do
    !$omp end distribute parallel do
    !$omp end teams
    do j = 1, 7
        do i = 1, 6
            if (d(i, j) /= 10*i + j) error stop
        end do
    end do

    s = 0
    !$omp teams num_teams(3) reduction(+:s)
    !$omp distribute parallel do reduction(+:s)
    do i = 1, 100
        s = s + i
    end do
    !$omp end distribute parallel do
    !$omp end teams
    if (s /= 5050) error stop

    m = 3
    n = 50
    e = 0
    !$omp teams num_teams(2)
    !$omp distribute parallel do
    do i = m, n
        e(i) = i
    end do
    !$omp end distribute parallel do
    !$omp end teams
    if (e(1) /= 0 .or. e(2) /= 0) error stop
    do i = 3, 50
        if (e(i) /= i) error stop
    end do

    print *, "ok"
end program
