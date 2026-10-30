program allocatable_09
    implicit none
    character(5), allocatable :: s
    character(0), allocatable :: empty
    character(-2), allocatable :: negative

    s = "abcde"
    call fill(s)
    call check(s, "hi   ")
    call truncate(s)
    call check(s, "longe")
    call recreate(s)
    call check(s, "x    ")
    deallocate(s)
    call check_unallocated(s, 5)
    call fill(s)
    call check(s, "hi   ")
    call allocate_out(s)
    call check(s, "out  ")
    deallocate(s)
    call allocate_out(s)
    call check(s, "out  ")
    call fill(empty)
    call check(empty, "")
    call allocate_out(empty)
    call check(empty, "")
    call fill(negative)
    call check(negative, "")
    call allocate_out(negative)
    call check(negative, "")
contains
    subroutine fill(t)
        character(*), allocatable, intent(inout) :: t
        integer :: original_length
        original_length = len(t)
        t = "hi"
        t = t
        if (len(t) /= original_length) error stop 5
    end subroutine

    subroutine truncate(t)
        character(*), allocatable, intent(inout) :: t
        t = "longer than five"
    end subroutine

    subroutine recreate(t)
        character(*), allocatable, intent(inout) :: t
        deallocate(t)
        allocate(t)
        if (len(t) /= 5) error stop 3
        call nested(t)
    end subroutine

    subroutine nested(t)
        character(*), allocatable, intent(inout) :: t
        t = "x"
    end subroutine

    subroutine allocate_out(t)
        character(*), allocatable, intent(out) :: t
        integer :: original_length
        if (allocated(t)) error stop 4
        original_length = len(t)
        allocate(t)
        t = "out"
        if (len(t) /= original_length) error stop 6
    end subroutine

    subroutine check_unallocated(t, expected)
        character(*), allocatable, intent(in) :: t
        integer, intent(in) :: expected
        if (allocated(t)) error stop 7
        if (len(t) /= expected) error stop 8
    end subroutine

    subroutine check(t, expected)
        character(*), intent(in) :: t, expected
        if (len(t) /= len(expected)) error stop 1
        if (t /= expected) error stop 2
    end subroutine
end program
