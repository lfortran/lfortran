program allocate_34
    implicit none

    type :: t1
        integer, allocatable :: arr(:)
    end type t1

    type :: t2
        type(t1), allocatable :: arr(:)
    end type t2

    type :: t3
        type(t2), allocatable :: z(:)
    end type t3

    type(t3) :: obj

    if (.not. allocated(obj%z)) then
        allocate(obj%z(2))
    end if

    if (.not. allocated(obj%z(1)%arr)) then
        allocate(obj%z(1)%arr(3))
    end if

    if (.not. allocated(obj%z(1)%arr(1)%arr)) then
        allocate(obj%z(1)%arr(1)%arr(5))
    end if

    if (size(obj%z(1)%arr(1)%arr) /= 5) error stop

end program
