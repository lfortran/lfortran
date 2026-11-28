module derived_types_vector_subscript_02_mod
    implicit none
    type :: slot
        integer :: key
        real :: val
        real :: arr(2)
    end type
    type :: outer
        type(slot) :: in
    end type
    type :: box
        type(slot) :: items(3)
    end type
contains
    subroutine fill(s, pool, idx)
        type(slot), intent(inout) :: s(:)
        real, intent(in) :: pool(:)
        integer, intent(in) :: idx(:)
        s%val = pool(idx)
    end subroutine

    subroutine fill_class(c, pool, idx)
        class(slot), intent(inout) :: c(:)
        real, intent(in) :: pool(:)
        integer, intent(in) :: idx(:)
        c%val = pool(idx)
    end subroutine
end module

program derived_types_vector_subscript_02
    use derived_types_vector_subscript_02_mod
    implicit none
    type(slot) :: s(3)
    type(slot), allocatable :: a(:)
    type(slot), target :: t(3)
    type(slot), pointer :: p(:)
    type(outer) :: o(3)
    type(box) :: x
    type(slot) :: g(3, 2)
    type(slot) :: low(0:2), none(0)
    real :: pool(5), got(3), m(4, 4), pool0(0:4)
    integer :: idx(3), i1(3), i2(2), idx5(5:7), empty(0), i, j

    pool = [1.5, 2.5, 3.5, 4.5, 5.5]
    idx = [4, 2, 5]

    ! the program of the issue
    s%key = [4, 2, 5]
    got = pool(s%key)
    if (any(got /= [4.5, 2.5, 5.5])) error stop

    ! the result goes into another component
    s%val = 0
    s%val = pool(s%key)
    print *, s%val
    if (any(s%val /= [4.5, 2.5, 5.5])) error stop

    s%val = 0
    s%val = pool(idx)
    if (any(s%val /= [4.5, 2.5, 5.5])) error stop
    s%val = pool(idx(3:1:-1))
    if (any(s%val /= [5.5, 2.5, 4.5])) error stop
    s%val = pool(s%key - 1)
    if (any(s%val /= [3.5, 1.5, 4.5])) error stop

    ! a section of the array below the component
    s(:)%val = pool([1, 2, 3])
    if (any(s%val /= [1.5, 2.5, 3.5])) error stop
    s(3:1:-1)%val = pool(idx)
    if (any(s%val /= [5.5, 2.5, 4.5])) error stop
    s(2:3)%val = pool(idx(1:2))
    if (any(s%val /= [5.5, 4.5, 2.5])) error stop

    ! an element of an array component
    s%arr(1) = 0
    s%arr(2) = pool(idx)
    print *, s%arr(2)
    if (any(s%arr(2) /= [4.5, 2.5, 5.5])) error stop
    if (any(s%arr(1) /= 0)) error stop

    ! a component of a component
    o%in%val = pool(idx)
    if (any(o%in%val /= [4.5, 2.5, 5.5])) error stop
    o%in%arr(1) = pool([1, 1, 2])
    if (any(o%in%arr(1) /= [1.5, 1.5, 2.5])) error stop

    ! a component of derived type
    s%key = [1, 2, 3]
    s%val = [10., 20., 30.]
    o%in = s([3, 1, 2])
    if (any(o%in%key /= [3, 1, 2])) error stop
    if (any(o%in%val /= [30., 10., 20.])) error stop

    ! an array component of a scalar
    x%items%val = pool(idx)
    if (any(x%items%val /= [4.5, 2.5, 5.5])) error stop

    ! allocatable, pointer and dummy arrays
    allocate(a(3))
    a%val = pool(idx)
    if (any(a%val /= [4.5, 2.5, 5.5])) error stop
    call fill(a, pool, [3, 3, 1])
    if (any(a%val /= [3.5, 3.5, 1.5])) error stop
    call fill(a(3:1:-2), pool, [4, 5])
    if (any(a%val /= [5.5, 3.5, 4.5])) error stop
    call fill_class(a, pool, [2, 1, 2])
    if (any(a%val /= [2.5, 1.5, 2.5])) error stop
    p => t(3:1:-1)
    p%val = pool([1, 2, 3])
    if (any(t%val /= [3.5, 2.5, 1.5])) error stop

    ! lower bounds other than 1, and no elements
    pool0 = pool
    idx5 = [3, 1, 4]
    low%val = pool0(idx5)
    if (any(low%val /= [4.5, 2.5, 5.5])) error stop
    none%val = pool(empty)
    if (size(none%val) /= 0) error stop

    ! rank 2
    m = reshape([(real(i), i = 1, 16)], [4, 4])
    i1 = [4, 1, 3]
    i2 = [3, 2]
    g%val = m(i1, i2)
    print *, g%val
    do j = 1, 2
        do i = 1, 3
            if (g(i, j)%val /= m(i1(i), i2(j))) error stop
        end do
    end do
    g%val = m(i1, 2:3)
    do j = 1, 2
        do i = 1, 3
            if (g(i, j)%val /= m(i1(i), j + 1)) error stop
        end do
    end do
    print *, "OK"
end program
