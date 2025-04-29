program template_03

    requirement op(T, U, V, op)
        type, deferred :: T
        type, deferred :: U
        type, deferred :: V
        elemental function op(a, b)
            type(T), intent(in) :: a
            type(U), intent(in) :: b
            type(V) :: op
        end function
    end requirement

    template axpy_tmpl(T, U, V, W, plus, times)
        public :: axpy
        require :: op(V, W, V, plus)
        require :: op(T, U, W, times)
    contains
        subroutine axpy(a, x, y)
            type(T), intent(in) :: a
            type(U), intent(in) :: x(:)
            type(V), intent(inout) :: y(:)
            y = plus(y, times(a, x))
        end subroutine
    end template

    call f()

contains
    
    elemental function my_mul(a, b) result(op)
        integer, parameter :: sp = kind(1.0)
        real(sp), intent(in) :: a
        integer, intent(in) :: b
        real(sp) :: op
        op = a * b
    end function

    elemental function my_add(a, b) result(op)
        integer, parameter :: sp = kind(1.0), dp = kind(1.d0)
        real(dp), intent(in) :: a
        real(sp), intent(in) :: b
        real(dp) :: op
        op = a + b
    end function

    subroutine my_axpy(a, x, y)
        integer, parameter :: sp = kind(1.0), dp = kind(1.d0)
        real(sp), intent(in) :: a
        integer, intent(in) :: x(:)
        real(dp), intent(inout) :: y(:)
        y = my_add(y, my_mul(a, x))
    end subroutine

    subroutine f()
        integer, parameter :: sp = kind(1.0), dp = kind(1.d0)
        instantiate axpy_tmpl(real(sp), integer, real(dp), real(sp), operator(+), operator(*))
        real(sp) :: a
        integer :: x(3)
        real(dp) :: y(3)
        a = 0.5
        x = 2
        y = 2
        call axpy(a, x, y)
        ! call my_axpy(a, x, y)   ! non-generic does not work too
    end subroutine
end program