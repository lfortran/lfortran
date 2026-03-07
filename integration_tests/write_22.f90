program write_22
    implicit none
    call test_implied_do_allocatable_string()
    contains
    subroutine test_implied_do_allocatable_string
        character(len=200) :: buf
        character(10), allocatable :: tmp_line
        integer :: i

        allocate(tmp_line)
        tmp_line = "Hello"
        write(buf, *) (tmp_line, i=1, 3)
        if (index(buf, "Hello") == 0) error stop
        if (index(buf, "HelloHello") == 0) then
            if (index(buf(11:), "Hello") == 0) error stop
        end if
    end subroutine
end program
