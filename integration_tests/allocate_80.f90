program allocate_80
    ! allocate(y, source=x) with deferred-length character scalars (#3640)
    implicit none
    character(len=:), allocatable :: x
    character(len=:), allocatable :: y

    allocate(character(len=5) :: x)
    x = "what!"

    allocate(y, source=x)
    deallocate(x)

    print *, allocated(x)
    print *, allocated(y), y
    if (allocated(x)) error stop
    if (.not. allocated(y)) error stop
    if (len(y) /= 5) error stop
    if (y /= "what!") error stop
    deallocate(y)
end program
