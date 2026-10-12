program random_number_02
    ! random_number with pointer arguments
    implicit none
    real, pointer :: p(:)
    double precision, pointer :: q(:,:)
    real, pointer :: s
    real, target :: t(6)
    real, pointer :: pt(:)
    integer :: i

    allocate(p(5))
    p = -1.0
    call random_number(p)
    do i = 1, size(p)
        if (p(i) < 0.0 .or. p(i) >= 1.0) error stop
    end do

    allocate(q(3, 4))
    q = -1.0d0
    call random_number(q)
    if (any(q < 0.0d0) .or. any(q >= 1.0d0)) error stop

    allocate(s)
    s = -1.0
    call random_number(s)
    if (s < 0.0 .or. s >= 1.0) error stop

    t = -1.0
    pt => t(2:5)
    call random_number(pt)
    if (t(1) /= -1.0 .or. t(6) /= -1.0) error stop
    if (any(t(2:5) < 0.0) .or. any(t(2:5) >= 1.0)) error stop

    deallocate(p, q, s)
    print *, "ok"
end program
