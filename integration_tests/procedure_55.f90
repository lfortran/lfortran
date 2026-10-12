! Interfaces whose dummy arguments depend on other dummy arguments, as
! interfaces of external procedures
module procedure_55_m
    implicit none
    abstract interface
        function str_iface(s) result(r)
            character(len=*), intent(in) :: s
            character(len=len(s)) :: r
        end function
    end interface
end module

real function procedure_55_sum(n, a)
    integer, intent(in) :: n
    real, intent(in) :: a(n)
    procedure_55_sum = sum(a)
end function

function procedure_55_upper(s) result(r)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: r
    integer :: i
    do i = 1, len(s)
        r(i:i) = achar(iachar(s(i:i)) - 32)
    end do
end function

subroutine procedure_55_rhs(n, x, f)
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    real, intent(out) :: f(n)
    f = -x
end subroutine

program procedure_55
    use procedure_55_m, only: str_iface
    implicit none
    abstract interface
        real function sum_iface(n, a)
            integer, intent(in) :: n
            real, intent(in) :: a(n)
        end function
        subroutine rhs_iface(n, x, f)
            integer, intent(in) :: n
            real, intent(in) :: x(n)
            real, intent(out) :: f(n)
        end subroutine
    end interface
    procedure(sum_iface) :: procedure_55_sum
    procedure(str_iface) :: procedure_55_upper
    procedure(rhs_iface) :: procedure_55_rhs
    real :: f(3)
    print *, procedure_55_sum(2, [1.0, 2.0])
    if (abs(procedure_55_sum(2, [1.0, 2.0]) - 3.0) > 1e-6) error stop
    print *, procedure_55_upper("abc")
    if (procedure_55_upper("abc") /= "ABC") error stop
    call procedure_55_rhs(3, [1.0, 2.0, 3.0], f)
    print *, f
    if (any(abs(f + [1.0, 2.0, 3.0]) > 1e-6)) error stop
end program
