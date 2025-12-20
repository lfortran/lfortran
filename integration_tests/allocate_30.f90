program allocate_30
    implicit none
    call test_fixed_length_char_array()
    call test_deferred_length_char_array()
    call test_zero_size_char_array()
contains
    subroutine test_fixed_length_char_array()
        character(len=10), allocatable :: from(:), to(:)
        allocate(from(3))
        from(1) = "Hello"
        from(2) = "World"
        from(3) = "Test"
        call move_alloc(from, to)
        if (allocated(from)) error stop
        if (.not. allocated(to)) error stop
        if (size(to) /= 3) error stop
        if (len(to) /= 10) error stop
        if (to(1) /= "Hello") error stop
        if (to(2) /= "World") error stop
        if (to(3) /= "Test") error stop
    end subroutine test_fixed_length_char_array

    subroutine test_deferred_length_char_array()
        character(len=:), allocatable :: from(:), to(:)
        allocate(character(len=5) :: from(2))
        from(1) = "12345"
        from(2) = "abcde"
        call move_alloc(from, to)
        if (allocated(from)) error stop
        if (.not. allocated(to)) error stop
        if (size(to) /= 2) error stop
        if (len(to) /= 5) error stop
        if (to(1) /= "12345") error stop
        if (to(2) /= "abcde") error stop
    end subroutine test_deferred_length_char_array

    subroutine test_zero_size_char_array()
        character(len=:), allocatable :: from(:), to(:)
        allocate(character(len=8) :: from(0))
        call move_alloc(from, to)
        if (allocated(from)) error stop
        if (.not. allocated(to)) error stop
        if (size(to) /= 0) error stop
        if (len(to) /= 8) error stop
    end subroutine test_zero_size_char_array

end program allocate_30