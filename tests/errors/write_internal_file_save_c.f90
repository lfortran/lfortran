program write_internal_file_save_c
    ! The C backend declares a character variable without `static`, so a
    ! `save` variable of a procedure would lose its value between calls.
    implicit none
    call f(1)
    call f(2)
contains
    subroutine f(n)
        integer, intent(in) :: n
        character(4), save :: s
        if (n == 2 .and. s /= "   1") error stop
        write(s, '(i4)') n
    end subroutine
end program
