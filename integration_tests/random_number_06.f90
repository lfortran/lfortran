program random_number_06
    implicit none
    integer, parameter :: qp = 16
    integer :: n, i
    integer, allocatable :: seed(:)
    real(qp) :: a(5), b(5)
    call random_seed(size=n)
    allocate(seed(n))
    seed = [(12345 + 7*i, i = 1, n)]
    call random_seed(put=seed)
    call random_number(a)
    call random_seed(put=seed)
    call random_number(b)
    if (any(a /= b)) error stop "real(16) sequence does not follow random_seed"
    print *, "ok"
end program
