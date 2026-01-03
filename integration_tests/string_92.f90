program string_92
    implicit none
    call test_concat('Upper', 'Diag')
    print *, "PASS"
contains
    subroutine test_concat(a, b)
        character, intent(in) :: a, b
        character(2) :: result
        result = a // b
        if (len(a) /= 1) error stop "len(a) should be 1"
        if (len(b) /= 1) error stop "len(b) should be 1"
        if (result /= 'UD') error stop "result should be 'UD'"
    end subroutine
end program
