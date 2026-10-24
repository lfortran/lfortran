program gpu_pointer_bound_01
    ! A do concurrent bound that is a pointer or allocatable integer scalar
    ! is evaluated once into a host temporary of the scalar integer type.
    implicit none
    real :: f(4)
    integer, target :: m
    integer, pointer :: mp
    integer, allocatable :: ma
    integer :: i

    m = 3
    mp => m
    f = 0
    do concurrent (i = 1:mp)
        f(i) = real(i)
    end do
    if (any(f /= [1., 2., 3., 0.])) error stop 1

    m = 2
    f = 0
    do concurrent (i = mp:4)
        f(i) = real(i)
    end do
    if (any(f /= [0., 2., 3., 4.])) error stop 2

    allocate(ma)
    ma = 3
    f = 0
    do concurrent (i = 1:ma)
        f(i) = real(i)
    end do
    if (any(f /= [1., 2., 3., 0.])) error stop 3

    ma = 2
    f = 0
    do concurrent (i = ma:mp + 1)
        f(i) = real(i)
    end do
    if (any(f /= [0., 2., 3., 0.])) error stop 4
    print *, "ok"
end program
