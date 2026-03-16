! Test that writes to a class(*) character array through
! a select type selector persist after end select.
program select_type_41
    implicit none
    class(*), allocatable :: arr(:)
    allocate(character(3) :: arr(2))

    ! Write through first select type block
    select type (arr)
    type is (character(*))
        arr(1) = "foo"
        arr(2) = "bar"
    end select

    ! Read back through second select type block
    select type (arr)
    type is (character(*))
        if (arr(1) /= "foo") error stop
        if (arr(2) /= "bar") error stop
    end select

    print *, "PASS"
end program
