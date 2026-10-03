program implied_do_loops27
    implicit none
    call test_test2()
contains
    subroutine test_test2
        character(len=20) :: tmp_line(3)
        character(len=20000) :: buf
        integer :: iii, ii

        tmp_line = "Hello"
        ii = 3

        write(buf, '(*(g0))') (trim(tmp_line(iii)), iii=1, ii)
        if (len_trim(buf) == 0) error stop
        if (trim(buf) /= "HelloHelloHello") error stop
    end subroutine
end program
