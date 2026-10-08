program random_seed_02
    ! random_seed with pointer arguments and array sections (#14125)
    implicit none
    integer, pointer :: n
    integer, pointer :: p(:)
    integer, allocatable :: g(:), h(:)
    integer :: i
    real :: x

    allocate(n)
    call random_seed(size=n)
    if (n <= 0) error stop "size must be positive"

    allocate(p(n), g(n), h(2*n))
    p = [(40 + i, i = 1, n)]
    call random_seed(put=p)
    call random_number(x)
    call random_seed(put=p)
    call random_seed(get=g)
    do i = 1, n
        if (g(i) /= p(i)) error stop "put with pointer array"
    end do

    h = 0
    h(1:2*n:2) = [(70 + i, i = 1, n)]
    call random_seed(put=h(1:2*n:2))
    call random_seed(get=g)
    do i = 1, n
        if (g(i) /= 70 + i) error stop "put with strided array section"
    end do

    h(n+1:2*n) = [(90 + i, i = 1, n)]
    call random_seed(put=h(n+1:2*n))
    call random_seed(get=g)
    do i = 1, n
        if (g(i) /= 90 + i) error stop "put with contiguous array section"
    end do
    print *, "ok"
end program random_seed_02
