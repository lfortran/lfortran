module pdt_09_module
  implicit none
  integer, parameter :: dp = kind(0.0d0)

  type tensor_t(k)
    integer, kind :: k = dp
    real(k) :: value
  end type tensor_t

  interface
    module subroutine trigger(self)
      class(tensor_t(dp)), intent(in) :: self
    end subroutine trigger
  end interface

end module pdt_09_module

program pdt_09
  use pdt_09_module, only : dp, tensor_t
  implicit none

  type(tensor_t(dp)) :: x

  x%value = 3.0_dp
  if (x%k /= dp) error stop
  if (abs(x%value - 3.0_dp) > 1.0e-12_dp) error stop
  print *, "ok"
end program pdt_09
