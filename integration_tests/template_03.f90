module template_03_m

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
    
    contains
    
    subroutine f()
        integer, parameter :: sp = kind(1.0), dp = kind(1.d0)
        instantiate axpy_tmpl(real, integer, real, real, operator(+), operator(*))
        real :: a
        integer :: x(3)
        real :: y(3)
        a = 0.5
        x = 2
        y = 0
        call axpy(a, x, y)
        print *, y
    end subroutine
    
end module
    
program template_03
use template_03_m, only: f
call f()
end program