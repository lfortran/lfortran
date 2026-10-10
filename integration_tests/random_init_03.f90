program random_init_03
    ! random_init with logical pointer arguments (#14125)
    implicit none
    logical, pointer :: rep, img
    integer, allocatable :: s1(:), s2(:)
    integer :: n
    real :: x, y

    allocate(rep, img)
    rep = .true.
    img = .false.
    call random_seed(size=n)
    allocate(s1(n), s2(n))

    call random_init(rep, img)
    call random_number(x)
    call random_seed(get=s1)
    call random_init(rep, img)
    call random_number(y)
    call random_seed(get=s2)
    if (x /= y) error stop "repeatable random_init via pointers"
    if (any(s1 /= s2)) error stop "repeatable random_init seeds via pointers"
    deallocate(rep, img)
    print *, "ok"
end program random_init_03
