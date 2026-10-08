program srand_01
    ! srand with an integer pointer argument (#14125)
    implicit none
    integer, pointer :: s
    real :: x, y

    allocate(s)
    s = 86456
    call srand(s)
    x = rand()
    call srand(s)
    y = rand()
    if (abs(x - y) > 1e-6) error stop "srand via pointer is not repeatable"
    print *, "ok"
end program srand_01
