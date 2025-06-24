module character_11_mod
    use iso_c_binding
    interface
        function f_string01_c(s) result(r) bind(c, name="f_string01_c")
            import c_char
            character(len=1, kind=c_char), intent(in) :: s
            character(len=1, kind=c_char) :: r
        end function
        
        function f_string02_c(s) result(r) bind(c, name="f_string02_c")
            import c_char
            character(len=1,kind=c_char), value, intent(in) :: s
            character(len=1, kind=c_char) :: r
        end function

        function f_string03_c(s, n) result(r) bind(c, name="f_string03_c") ! eqivalent to f_string01_c
            import :: c_int, c_char
            integer(c_int), intent(in) :: n
            character(len=1, kind=c_char),intent(in) :: s(n)
            character(len=1, kind=c_char) :: r
        end function

        function f_string04_c(s) result(r) bind(c, name="f_string04_c") ! eqivalent to f_string01_c
            import :: c_char
            character(len=1, kind=c_char),intent(in) :: s(*)
            character(len=1, kind=c_char) :: r
        end function

        function f_string05_c(s) result(r) bind(c, name="f_string05_c") ! eqivalent to f_string01_c (but argument is an ISO descriptor)
            import :: c_char
            character(len=1, kind=c_char),intent(in) :: s(:)
            character(len=1, kind=c_char) :: r
        end function
    end interface
contains 
! Expected Behavior : 
! C Implementation printing the passed message (Whole message, Although the interface says len = 1. It's due to being passed by reference `char*`) + 
! The return should be the first character of the passed string +
! Second char in passed argument should be modified
subroutine f_string01_test()
    character(len=3) :: s
    character(len=2) :: r
    s = "Hi" // c_null_char

    r = f_string01_c(s)

    print "( '[', a, ']' )", r
    if(r /= "H ") error stop

    print "( '[', a, ']' )", s
    if(s(1:1) /= "H") error stop 
    if(s(2:2) /= "X") error stop 
end subroutine 

! Expected Behavior : 
! C Implementation printing only first character (due to passing by value) + 
! The return should be the first character of the passed string
subroutine f_string02_test()
    character(len=3) :: s
    character(len=2) :: r
    s = "Hi" // c_null_char

    r = f_string02_c(s)

    print "( '[', a, ']' )", r
    if(r /= "H ") error stop

    print "( '[', a, ']' )", s
end subroutine 


! Expected Behavior : 
! C Implementation printing the passed message + 
! The return should be the first character of the passed string +
! Second char in passed argument should be modified By C's implementation
subroutine f_string03_test()
    character(len=4) :: s
    character(len=2) :: r
    s = "Bye" // c_null_char
! TODO : Fix this
    ! r = f_string03_c(s, 100000) ! Array's size is irrelevant

    print "( '[', a, ']' )", r
    if(r /= "B ") error stop

    print "( '[', a, ']' )", s
    if(s(1:1) /= "B") error stop
    if(s(2:2) /= "X") error stop
    if(s(3:3) /= "e") error stop
end subroutine 

! Expected Behavior : 
! C Implementation printing the passed message + 
! The return should be the first character of the passed string +
! Second char in passed argument should be modified By C's implementation
subroutine f_string04_test()
    character(len=4) :: s
    character(len=2) :: r
    s = "Bye" // c_null_char
    
    r = f_string04_c(s)

    print "( '[', a, ']' )", r
    if(r /= "B ") error stop
    
    print "( '[', a, ']' )", s
    if(s(1:1) /= "B") error stop
    if(s(2:2) /= "X") error stop
    if(s(3:3) /= "e") error stop
end subroutine 

! Expected Behavior : 
! C Implementation printing the passed message + 
! The return should be the first character of the passed string +
! Second char in passed argument should be modified By C's implementation
subroutine f_string05_test()
    character(len=4) :: s(1)
    character(len=2) :: r
    s(1) = "Bye" // c_null_char
    r = f_string05_c(s)

    print "( '[', a, ']' )", r
    if(r /= "B ") error stop
    
    print "( '[', a, ']' )", s
    if(s(1)(1:1) /= "B") error stop
    if(s(1)(2:2) /= "X") error stop
    if(s(1)(3:3) /= "e") error stop
end subroutine 
end module 


program character_11
    call f_string01_test()
    call f_string02_test()
    call f_string03_test()
    call f_string04_test()
    call f_string05_test()
end program