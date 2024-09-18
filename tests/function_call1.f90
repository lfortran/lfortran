module module_function_call1
    type :: softmax
    contains
      procedure :: eval_1d
    end type softmax
  contains
  
    pure function eval_1d(self, x) result(res)
      class(softmax), intent(in) :: self
      real, intent(in) :: x(:)
      real :: res(size(x))
    end function eval_1d
  
    pure function eval_1d_prime(self, x) result(res)
      class(softmax), intent(in) :: self
      real, intent(in) :: x(:)
      real :: res(size(x))
      res = self%eval_1d(x)
    end function eval_1d_prime
end module module_function_call1
