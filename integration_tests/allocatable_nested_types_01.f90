program allocatable_nested_types_01
    implicit none

    type :: t1
        integer, allocatable :: ll
    end type

    type :: t2
        type(t1), allocatable :: arr(:)
    end type t2

    type :: t3
        type(t2), allocatable :: z(:)
    end type

    call ss

contains
    subroutine ss
        type(t3), allocatable :: t2
        allocate(t2)
        if (.not. allocated(t2)) error stop
        allocate(t2%z(10))
        if (size(t2%z) /= 10) error stop
        allocate(t2%z(1)%arr(5))
        if (size(t2%z(1)%arr) /= 5) error stop
    end subroutine

end program
