! Local array bounds that call a function returning a derived type and also
! reference several host- or use-associated variables.
module spec_expr_04_a
    implicit none
    integer :: n = 3, m = 4
    type :: t
        integer :: v = 1
    end type
contains
    pure function make(i) result(r)
        integer, intent(in) :: i
        type(t) :: r
        r%v = i
    end function
    pure integer function value(x)
        type(t), intent(in) :: x
        value = x%v
    end function
    subroutine run()
        real :: a(value(make(1)) + m)
        real :: b(value(make(n)) + m)
        a = 1
        b = 2
        if (size(a) /= 5) error stop 1
        if (size(b) /= 7) error stop 2
        if (abs(sum(b) - 14) > 1e-6) error stop 3
    end subroutine
end module

module spec_expr_04_b
    implicit none
    integer :: n = 10
end module

module spec_expr_04_c
    use spec_expr_04_a, only: t, make, value, na => n
    use spec_expr_04_b, only: nb => n
    implicit none
contains
    subroutine run_renamed()
        integer :: a(value(make(na)) + nb + na)
        integer :: b(value(make(nb)) - na)
        if (size(a) /= 16) error stop 4
        if (size(b) /= 7) error stop 5
    end subroutine
    subroutine run_host(k)
        integer, intent(in) :: k
        integer :: j
        j = 2
        call inner()
    contains
        subroutine inner()
            integer :: c(value(make(k)) + j + nb)
            if (size(c) /= 17) error stop 6
        end subroutine
    end subroutine
end module

program spec_expr_04
    use spec_expr_04_a, only: run
    use spec_expr_04_c, only: run_renamed, run_host
    implicit none
    call run()
    call run_renamed()
    call run_host(5)
    print *, "ok"
end program
