! A generic interface and an operator declared in a procedure with an ENTRY statement
subroutine other(i)
    integer, intent(inout) :: i
    i = i + 100
end subroutine

integer function plus(a, b)
    integer, intent(in) :: a, b
    plus = a + b + 1000
end function

subroutine s(i)
    integer, intent(inout) :: i
    interface gen
        subroutine other(i)
            integer, intent(inout) :: i
        end subroutine
    end interface
    interface operator(.plus.)
        integer function plus(a, b)
            integer, intent(in) :: a, b
        end function
    end interface
    call gen(i)
    i = i .plus. 1
    return
entry e(i)
    i = i + 10
end subroutine

subroutine r(i)
    integer, intent(inout) :: i
    interface gen
        procedure r
    end interface
    i = i + 1
    if (i < 5) call gen(i)
    return
entry f(i)
    i = i + 10
end subroutine

program entry_generic_interface_01
    integer :: i
    i = 0
    call s(i)
    if (i /= 1101) error stop
    call e(i)
    if (i /= 1111) error stop
    i = 0
    call r(i)
    if (i /= 5) error stop
    call f(i)
    if (i /= 15) error stop
    print *, i
end program
