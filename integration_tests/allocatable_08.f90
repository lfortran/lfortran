program allocatable_08
    implicit none
    character(5), allocatable :: s
    character(3), allocatable :: automatic
    character(0), allocatable :: empty
    character(-2), allocatable :: negative

    allocate(s)
    call check_length(s, 5)
    s = "hi"
    call check_value(s, "hi   ")
    s = "longer than five"
    call check_value(s, "longe")
    s = s
    call check_value(s, "longe")
    deallocate(s)
    s = "x"
    call check_value(s, "x    ")
    automatic = "abcdef"
    call check_value(automatic, "abc")
    allocate(empty, negative)
    call check_length(empty, 0)
    call check_length(negative, 0)
    empty = "x"
    negative = "x"
    call check_length(empty, 0)
    call check_length(negative, 0)
contains
    subroutine check_length(t, expected)
        character(*), intent(in) :: t
        integer, intent(in) :: expected
        if (len(t) /= expected) error stop 1
    end subroutine

    subroutine check_value(t, expected)
        character(*), intent(in) :: t, expected
        if (len(t) /= len(expected)) error stop 2
        if (t /= expected) error stop 3
    end subroutine
end program
