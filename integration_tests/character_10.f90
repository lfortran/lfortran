
module module_10_mod
    use iso_c_binding
    implicit none

    interface 
        function f_string00_c_caller() result(r) bind(c)
            import :: c_int
            integer(c_int) :: r
        end function
        function f_string01_c_caller() result(r) bind(c)
            import :: c_int
            integer(c_int) :: r
        end function
        function f_string02_c_caller() result(r) bind(c)
            import :: c_int
            integer(c_int) :: r
        end function
        function f_string03_c_caller() result(r) bind(c)
            import :: c_int
            integer(c_int) :: r
        end function
        function f_string04_c_caller() result(r) bind(c)
            import :: c_int
            integer(c_int) :: r
        end function
        function f_string05_c_caller() result(r) bind(c)
            import :: c_int
            integer(c_int) :: r
        end function
    end interface 

    contains 
    function f_string00_fortran(s) result(r) bind(c)
        character(kind=c_char) :: s
        character(kind=c_char) :: r
        print "('Fortran Side : `f_string00_fortran` called with s =', '[', a, ']' )", s
        if(s /= 'H') error stop
        r = s
        s(1:1) = 'X'
    end function
    
    function f_string01_fortran(s) result(r) bind(c)
        character(len=1, kind=c_char), value :: s
        character(len=1, kind=c_char) :: r
        print "('Fortran Side : `f_string01_fortran` called with s =', '[', a, ']' )", s
        if(s /= 'H') error stop
        r = s
        s = 'X'
    end function

    function f_string02_fortran(s, n) result(r) bind(c)
        integer(c_int), value :: n
        character(len=1, kind=c_char) :: s(n)
        character(len=1, kind=c_char) :: r
        character(len=:, kind=c_char), allocatable :: local_str
        print "('Fortran Side : `f_string02_fortran` called with s =', '[', a, ']' )", s(1)
        if(s(1) /= "A") error stop

        local_str = s(1) // "?????????????"
        if(local_str /= "A?????????????") error stop
        if(len(local_str) /= 14) error stop
        
        r = local_str
        s(1) = '2'
    end function

    function f_string03_fortran(s) result(r) bind(c)
        character(len=1, kind=c_char) :: s(*)
        character(len=1, kind=c_char) :: r
        print "('Fortran Side : `f_string03_fortran` called with s =', '[', a, ']' )", s(1)
        if(s(1) /= 'B') error stop
        r = s(1)
        s(1) = '3'
    end function

    function f_string04_fortran(s) result(r) bind(c)
        character(len=1, kind=c_char) :: s(*)
        character(len=1, kind=c_char) :: r
        print "('Fortran Side : `f_string04_fortran` called with s =', '[', a, ']' )", s(1)
        if(s(1) /= 'C') error stop
        r = s(1)
        s(1) = '4'
    end function

    function f_string05_fortran(s) result(r) bind(c)
        character(len=1, kind=c_char) :: s(:)
        character(len=1, kind=c_char) :: r
        print "('Fortran Side : `f_string05_fortran` called with s =', '[', a, ']' )", s(1)
        if(s(1) /= 'D') error stop
        r = s(1)
        s(1) = '5'
    end function


end module 

program module_10
 use module_10_mod
 integer :: dummy
    dummy = f_string00_c_caller()
    dummy = f_string01_c_caller()
    dummy = f_string02_c_caller()
    dummy = f_string03_c_caller()
    dummy = f_string04_c_caller()
    dummy = f_string05_c_caller()
end program