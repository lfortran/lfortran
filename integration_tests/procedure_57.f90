! An external procedure declared with a procedure declaration statement in
! a BLOCK construct
real function procedure_57_twice(x)
    real, intent(in) :: x
    procedure_57_twice = 2*x
end function

subroutine procedure_57_inc(i)
    integer, intent(inout) :: i
    i = i + 1
end subroutine

subroutine procedure_57_sub(r)
    implicit none
    real, intent(out) :: r
    abstract interface
        real function f_iface(x)
            real, intent(in) :: x
        end function
    end interface
    block
        block
            procedure(f_iface) :: procedure_57_twice
            r = procedure_57_twice(3.0)
        end block
    end block
end subroutine

program procedure_57
    implicit none
    abstract interface
        real function f_iface(x)
            real, intent(in) :: x
        end function
        subroutine s_iface(i)
            integer, intent(inout) :: i
        end subroutine
    end interface
    integer :: i
    real :: r
    i = 1
    block
        procedure(f_iface) :: procedure_57_twice
        procedure(s_iface) :: procedure_57_inc
        print *, procedure_57_twice(2.0)
        if (abs(procedure_57_twice(2.0) - 4.0) > 1e-6) error stop
        call procedure_57_inc(i)
    end block
    print *, i
    if (i /= 2) error stop
    call procedure_57_sub(r)
    print *, r
    if (abs(r - 6.0) > 1e-6) error stop
end program
