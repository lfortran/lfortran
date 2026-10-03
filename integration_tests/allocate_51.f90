program allocate_51
    implicit none
    type :: my_type
        integer :: max_len = 0
    end type
    type(my_type) :: obj
    character(:), allocatable :: names(:)

    obj%max_len = 5
    allocate(character(obj%max_len)::names(3))

    if (len(names) /= 5) error stop
    if (size(names) /= 3) error stop

    names(1) = "hello"
    names(2) = "world"
    names(3) = "abcde"

    if (names(1) /= "hello") error stop
    if (names(2) /= "world") error stop
    if (names(3) /= "abcde") error stop

    print *, "PASS"
end program
