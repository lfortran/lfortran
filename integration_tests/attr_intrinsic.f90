! this program tests 'intrinsic' attribute used in declarations
program attr_intrinsic
    implicit none
    ! variable 'abs' with same name as intrinsic elemental
    ! function 'abs'
    integer, parameter :: abs = 11
    integer :: i1, i2

    ! variable 'char' with same name as intrinsic elemental
    ! function 'char'
    integer, parameter :: char = 19

    call sub1()
    i1 = fun1()
    call sub2()
    i2 = fun2()

    contains

    subroutine sub1()
        ! intrinsic 'abs' means, 'abs' used in this subroutine
        ! is actually the intrinsic elemental function 'abs'
        intrinsic abs
        print *, abs(-4)
        if (abs(-4) /= 4) error stop
    end subroutine sub1

    function fun1() result(i)
        ! dummy result output
        integer :: i
        ! the 'char' here is the variable 'char' not the intrinsic
        ! function
        print *, 'variable char: ', char
        if (char /= 19) error stop
    end function fun1

    subroutine sub2()
        print *, abs
        if (abs /= 11) error stop
    end subroutine sub2

    function fun2() result(i)
        ! dummy result output
        integer :: i
        intrinsic char
        ! the 'char' here is the intrinsic function 'char' not
        ! the variable 'char'
        print *, 'intrinsic elemental char(65): ', char(65)
        if (char(65) /= 'A') error stop
    end function fun2
end program attr_intrinsic
