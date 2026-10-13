module derived_type_borrow_01_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    implicit none
    type(c_ptr) :: expected
    type :: Payload
        integer :: n
    end type
    type :: Holder
        type(Payload) :: item
        type(Payload), pointer :: pointer_item
        type(Payload), allocatable :: allocated_item
    end type
contains
    function observe(object) result(r)
        type(Payload), target, intent(in) :: object
        integer :: r
        type(c_ptr) :: actual
        actual = c_loc(object%n)
        if (.not. c_associated(actual, expected)) error stop 1
        r = object%n
    end function
end module

program derived_type_borrow_01
    use derived_type_borrow_01_m
    implicit none
    type(Holder), target :: container
    type(Payload), target :: items(2), scalar
    type(Payload), pointer :: alias
    type(Payload), allocatable, target :: allocation
    integer :: i
    container%item%n = 17
    expected = c_loc(container%item%n)
    if (observe(container%item) /= 17) error stop 2
    do i = 1, 2
        items(i)%n = 20 + i
        expected = c_loc(items(i)%n)
        if (observe(items(i)) /= 20 + i) error stop 3
    end do
    scalar%n = 31
    expected = c_loc(scalar%n)
    if (observe(scalar) /= 31) error stop 4
    alias => scalar
    if (observe(alias) /= 31) error stop 5
    container%pointer_item => scalar
    if (observe(container%pointer_item) /= 31) error stop 7
    scalar%n = 37
    if (observe(container%pointer_item) /= 37) error stop 8
    nullify(container%pointer_item)
    allocate(container%allocated_item)
    container%allocated_item%n = 47
    expected = c_loc(container%allocated_item%n)
    ! The ordinary TARGET-dummy control uses an alias of the same owned payload.
    alias => container%allocated_item
    if (observe(alias) /= 47) error stop 9
    nullify(alias)
    deallocate(container%allocated_item)
    allocate(allocation)
    allocation%n = 41
    expected = c_loc(allocation%n)
    if (observe(allocation) /= 41) error stop 6
    deallocate(allocation)
end program
