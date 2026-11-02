program allocatable_polymorphic_mold_03
    implicit none
    integer, parameter :: n = 2000000
    real :: a(n)
    integer :: b(2, 3)
    class(*), pointer :: t(:)
    class(*), allocatable :: u(:, :)

    allocate(t, mold=a)
    if (size(t) /= n) error stop
    if (lbound(t, 1) /= 1 .or. ubound(t, 1) /= n) error stop
    select type (t)
    type is (real)
        t(n) = 1.5
        if (t(n) /= 1.5) error stop
    class default
        error stop
    end select
    deallocate(t)

    allocate(u, mold=b)
    if (any(shape(u) /= [2, 3])) error stop
    select type (u)
    type is (integer)
        u = 7
        if (any(u /= 7)) error stop
    class default
        error stop
    end select
    print *, "ok"
end program allocatable_polymorphic_mold_03
