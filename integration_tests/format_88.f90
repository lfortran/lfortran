program format_88
    implicit none
    character(len=30) :: wr
    character(len=18) :: poison
    character(len=10) :: hello_god
    character(len=*), parameter :: expected = "                    Hello God!"

    hello_god = "Hello God!"

    write(poison, "(A)") "12345678901234567" // new_line("a")
    print *, poison
    write(wr, "(TR23, TL3, A)") hello_god
    print *, wr, hello_god
    if (wr /= expected) then
        print *, "actual  = '", wr, "'"
        print *, "expected = '", expected, "'"
        error stop
    end if
end program format_88
