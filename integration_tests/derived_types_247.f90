program derived_types_247
    ! Intrinsic assignment of a derived type with an allocatable array
    ! component of its own type makes an independent deep copy (#14189)
    implicit none
    type :: node
        integer :: val = 0
        type(node), allocatable :: kids(:)
    end type
    type(node) :: x, y, z

    x%val = 1
    allocate(x%kids(3))
    x%kids(1)%val = 10
    x%kids(2)%val = 20
    x%kids(3)%val = 30
    allocate(x%kids(2)%kids(2))
    x%kids(2)%kids(1)%val = 210
    x%kids(2)%kids(2)%val = 220

    y = x
    ! Modifying the copy does not change the original
    y%val = -1
    y%kids(1)%val = -10
    y%kids(2)%kids(2)%val = -220
    allocate(y%kids(3)%kids(4))
    deallocate(y%kids(2)%kids)
    if (x%val /= 1) error stop 1
    if (x%kids(1)%val /= 10) error stop 2
    if (.not. allocated(x%kids(2)%kids)) error stop 3
    if (x%kids(2)%kids(2)%val /= 220) error stop 4
    if (allocated(x%kids(3)%kids)) error stop 5

    ! Modifying the original does not change the copy
    x%kids(3)%val = 33
    if (y%kids(3)%val /= 30) error stop 6
    if (size(y%kids(3)%kids) /= 4) error stop 7

    ! Assignment over an already allocated destination
    z = y
    z = x
    if (z%val /= 1) error stop 8
    if (size(z%kids) /= 3) error stop 9
    if (z%kids(1)%val /= 10) error stop 10
    if (z%kids(2)%kids(2)%val /= 220) error stop 11
    if (allocated(z%kids(3)%kids)) error stop 12
    z%kids(2)%kids(1)%val = 0
    if (x%kids(2)%kids(1)%val /= 210) error stop 13

    print *, "ok"
end program
