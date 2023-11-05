program template_03

    requirement op(T, U, V, op)
        type, deferred :: T
        type, deferred :: U
        type, deferred :: V
        interface
            elemental function op(a, b)
            type(T), intent(in) :: a
            type(U), intent(in) :: b
            type(V) :: op
            end function
        end interface
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
        integer :: i
        do i = 1, size(x)
            y(i) = plus(y(i), times(a, x(i)))
        end do
        end subroutine
    end template

    call f()

contains
    
    subroutine f()
        integer, parameter :: sp = kind(1.0), dp = kind(1.d0)
        instantiate axpy_tmpl(real(sp), integer, real(dp), real(sp), operator(+), operator(*))
        real(sp) :: a
        integer :: x(3)
        real(dp) :: y(3)
        a = 0.5
        x = 2
        y = 0
        call axpy(a, x, y)
        print *, y
    end subroutine
end program