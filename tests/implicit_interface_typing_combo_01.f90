! Regression: --implicit-interface and --implicit-typing together used to
! segfault when a module with 'implicit none' called an undeclared function.
module m
   implicit none
contains
   subroutine s(x, y)
      real(8), intent(out) :: x
      real(8), intent(in)  :: y
      x = unknown_func(y)
   end subroutine
end module
