! The interface of a procedure declaration statement of a subroutine or a
! function is a procedure named like a specific intrinsic function that is
! declared after the statement: by an interface body later in the
! specification part or by an internal procedure of the subroutine
subroutine procedure_59_s1(x, r)
    implicit none
    real, intent(in) :: x
    integer, intent(out) :: r
    procedure(sin) :: procedure_59_f
    interface
        integer function sin(x)
            real, intent(in) :: x
        end function
    end interface
    r = procedure_59_f(x)
end subroutine

subroutine procedure_59_s2(x, r)
    implicit none
    real, intent(in) :: x
    integer, intent(out) :: r
    procedure(sin) :: procedure_59_f
    r = procedure_59_f(x) + sin(x)
contains
    integer function sin(x)
        real, intent(in) :: x
        sin = 100
    end function
end subroutine

subroutine procedure_59_s3(f, x, r)
    implicit none
    procedure(abs) :: f
    real, intent(in) :: x
    integer, intent(out) :: r
    r = f(x)
contains
    integer function abs(x)
        real, intent(in) :: x
        abs = 0
    end function
end subroutine

integer function procedure_59_g(i)
    implicit none
    integer, intent(in) :: i
    procedure(sqrt) :: procedure_59_h
    interface
        integer function sqrt(i)
            integer, intent(in) :: i
        end function
    end interface
    procedure_59_g = procedure_59_h(i)
end function

integer function procedure_59_f(x)
    implicit none
    real, intent(in) :: x
    procedure_59_f = 7 + nint(x)
end function

integer function procedure_59_h(i)
    implicit none
    integer, intent(in) :: i
    procedure_59_h = 10*i
end function

program procedure_59
    implicit none
    interface
        subroutine procedure_59_s1(x, r)
            real, intent(in) :: x
            integer, intent(out) :: r
        end subroutine
        subroutine procedure_59_s2(x, r)
            real, intent(in) :: x
            integer, intent(out) :: r
        end subroutine
        subroutine procedure_59_s3(f, x, r)
            interface
                integer function f(x)
                    real, intent(in) :: x
                end function
            end interface
            real, intent(in) :: x
            integer, intent(out) :: r
        end subroutine
        integer function procedure_59_g(i)
            integer, intent(in) :: i
        end function
        integer function procedure_59_f(x)
            real, intent(in) :: x
        end function
    end interface
    integer :: r

    call procedure_59_s1(2.0, r)
    print *, r
    if (r /= 9) error stop

    call procedure_59_s2(3.0, r)
    print *, r
    if (r /= 110) error stop

    call procedure_59_s3(procedure_59_f, 4.0, r)
    print *, r
    if (r /= 11) error stop

    r = procedure_59_g(3)
    print *, r
    if (r /= 30) error stop
end program
