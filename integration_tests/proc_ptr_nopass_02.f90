module proc_ptr_nopass_02_mod
  implicit none

  abstract interface
    function compute_func(x, scale, verbose) result(y)
      real, intent(in) :: x
      real, intent(in) :: scale
      integer, intent(in), optional :: verbose
      real :: y
    end function compute_func
  end interface

  type :: functor_type
    procedure(compute_func), nopass, pointer :: fn_ptr => null()
  end type functor_type

contains

  function scale_x(x, scale, verbose) result(y)
    real, intent(in) :: x
    real, intent(in) :: scale
    integer, intent(in), optional :: verbose
    real :: y
    y = x * scale
  end function scale_x

  ! Calling a procedure-pointer component with keyword args used to trigger:
  ! AssertFailed: ASR::is_a<ASR::GenericProcedure_t>(*f2)
  subroutine run_functor(f, input, output)
    type(functor_type), intent(in) :: f
    real, intent(in) :: input
    real, intent(out) :: output
    output = f%fn_ptr(input, scale=2.0, verbose=0)
  end subroutine run_functor

end module proc_ptr_nopass_02_mod

program proc_ptr_nopass_02
  use proc_ptr_nopass_02_mod
  implicit none
  type(functor_type) :: f
  real :: result_val

  f%fn_ptr => scale_x
  call run_functor(f, 3.0, result_val)
  if (abs(result_val - 6.0) > 1e-5) error stop "Expected 3.0 * 2.0 = 6.0"

  call run_functor(f, 5.0, result_val)
  if (abs(result_val - 10.0) > 1e-5) error stop "Expected 5.0 * 2.0 = 10.0"
end program proc_ptr_nopass_02
