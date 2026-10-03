module interface_30_mod
   interface
      pure function scalar_1D_initializer_i(x) result(res)
         double precision, intent(in) :: x(:)
         double precision, allocatable :: res(:)
      end function
   end interface
end module

module interface_30_functions_m
   implicit none

contains

   pure function f(x)
      double precision, intent(in) :: x(:)
      double precision, allocatable :: f(:)
      allocate(f(size(x)))
      f = x**2
   end function

end module interface_30_functions_m

program interface_30
   use interface_30_functions_m
   use interface_30_mod
   implicit none
   procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => f
   double precision, allocatable :: x(:), res(:)
   integer :: n
   n = 5
   allocate(x(n))
   x = [1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0]
   res = scalar_1D_initializer(x)
   print *, res
   if (all(res /= [1.0d0, 4.0d0, 9.0d0, 16.0d0, 25.0d0])) error stop
   deallocate(x, res)
end program interface_30
