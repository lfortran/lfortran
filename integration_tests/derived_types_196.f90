! Elements of an array of a derived type must keep the default initialization
! of a `type(c_ptr)` array component, and an intrinsic assignment of such a
! type must copy every element of that component.
program derived_types_196
    use iso_c_binding, only: c_ptr, c_null_ptr, c_loc, c_associated
    implicit none

    type :: t
        type(c_ptr) :: p(2) = c_null_ptr
        integer :: g(2) = 7
        type(c_ptr) :: s = c_null_ptr
        integer :: h = 0
    end type

    type :: outer
        type(t) :: in
        type(c_ptr), allocatable :: al(:)
        integer :: k = 3
    end type

    type(t) :: arr(2), mat(2,2), a, b
    type(outer) :: o1, o2
    integer, target :: tg1, tg2
    integer :: i, j

    ! array of the type, filled from an array constructor of structure
    ! constructors: every element keeps the c_null_ptr default
    arr = [t(h=1), t(h=2)]
    if (arr(1)%h /= 1) error stop
    if (arr(2)%h /= 2) error stop
    do i = 1, 2
        do j = 1, 2
            if (c_associated(arr(i)%p(j))) error stop
        end do
        if (c_associated(arr(i)%s)) error stop
        if (arr(i)%g(1) /= 7) error stop
        if (arr(i)%g(2) /= 7) error stop
    end do

    ! rank-2 array of the type, broadcast from a scalar structure constructor
    mat = t(h=5)
    do i = 1, 2
        do j = 1, 2
            if (mat(i,j)%h /= 5) error stop
            if (c_associated(mat(i,j)%p(1))) error stop
            if (c_associated(mat(i,j)%p(2))) error stop
        end do
    end do

    ! a scalar of the type keeps working
    a = t(h=4)
    if (a%h /= 4) error stop
    if (c_associated(a%p(1))) error stop
    if (c_associated(a%p(2))) error stop

    ! intrinsic assignment copies every element of the c_ptr array component
    a%p(1) = c_loc(tg1)
    a%p(2) = c_loc(tg2)
    a%s = c_loc(tg1)
    a%g(2) = 11
    a%h = 9
    b = a
    if (b%h /= 9) error stop
    if (b%g(1) /= 7) error stop
    if (b%g(2) /= 11) error stop
    if (.not. c_associated(b%p(1), c_loc(tg1))) error stop
    if (.not. c_associated(b%p(2), c_loc(tg2))) error stop
    if (.not. c_associated(b%s, c_loc(tg1))) error stop

    ! nested type and an allocatable c_ptr array component
    allocate(o1%al(3))
    o1%al = c_null_ptr
    o1%al(2) = c_loc(tg2)
    o1%in%p(1) = c_loc(tg1)
    o1%k = 7
    o2 = o1
    if (o2%k /= 7) error stop
    if (.not. c_associated(o2%in%p(1), c_loc(tg1))) error stop
    if (c_associated(o2%in%p(2))) error stop
    if (c_associated(o2%in%s)) error stop
    if (.not. allocated(o2%al)) error stop
    if (size(o2%al) /= 3) error stop
    if (c_associated(o2%al(1))) error stop
    if (.not. c_associated(o2%al(2), c_loc(tg2))) error stop
    if (c_associated(o2%al(3))) error stop

    print *, "ok"
end program derived_types_196
