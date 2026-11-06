program derived_types_246
    ! Intrinsic assignment of a derived type with an allocatable array
    ! component of its own type (#14189)
    implicit none
    type :: node
        integer :: val = 0
        type(node), allocatable :: kids(:)
    end type
    type(node) :: x, y

    ! Unallocated component
    y = x
    if (allocated(y%kids)) error stop 1
    if (y%val /= 0) error stop 2

    ! Two levels of nesting
    x%val = 1
    allocate(x%kids(3))
    x%kids(1)%val = 10
    x%kids(2)%val = 20
    x%kids(3)%val = 30
    allocate(x%kids(2)%kids(2))
    x%kids(2)%kids(1)%val = 210
    x%kids(2)%kids(2)%val = 220

    y = x
    if (y%val /= 1) error stop 3
    if (.not. allocated(y%kids)) error stop 4
    if (size(y%kids) /= 3) error stop 5
    if (y%kids(1)%val /= 10) error stop 6
    if (y%kids(2)%val /= 20) error stop 7
    if (y%kids(3)%val /= 30) error stop 8
    if (allocated(y%kids(1)%kids)) error stop 9
    if (.not. allocated(y%kids(2)%kids)) error stop 10
    if (size(y%kids(2)%kids) /= 2) error stop 11
    if (y%kids(2)%kids(1)%val /= 210) error stop 12
    if (y%kids(2)%kids(2)%val /= 220) error stop 13
    if (allocated(y%kids(3)%kids)) error stop 14

    ! Structure constructor with nested array constructors
    y = node(5, [node(6), node(7, [node(8), node(9), node(10)])])
    if (y%val /= 5) error stop 15
    if (size(y%kids) /= 2) error stop 16
    if (y%kids(1)%val /= 6) error stop 17
    if (allocated(y%kids(1)%kids)) error stop 18
    if (size(y%kids(2)%kids) /= 3) error stop 19
    if (y%kids(2)%kids(3)%val /= 10) error stop 20

    print *, "ok"
end program
