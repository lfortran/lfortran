! Forwarding an assumed-shape array with a non-default lower bound
! through a procedure pointer call. The caller has bounds (lb-1:, ...)
! and forwards the array via pp(lb, arr) to a callee that also declares
! bounds (lb-1:, ...). pass_array_by_data must pass the array's extent
! (size), not its upper bound, otherwise the callee computes the wrong
! upper bound for arrays whose lower bound is not 1.
module proc_ptr_18_mod
  abstract interface
    subroutine iface(lb, arr)
      integer, intent(in) :: lb
      real(8), intent(in) :: arr(lb-1:, lb-1:, :, :)
    end subroutine
  end interface
contains
  subroutine target_sub(lb, arr)
    integer, intent(in) :: lb
    real(8), intent(in) :: arr(lb-1:, lb-1:, :, :)
    if (lbound(arr,1) /= lb-1) error stop "lb1"
    if (ubound(arr,1) /= lb+1) error stop "ub1"
    if (lbound(arr,2) /= lb-1) error stop "lb2"
    if (ubound(arr,2) /= lb+1) error stop "ub2"
    if (lbound(arr,3) /= 1) error stop "lb3"
    if (ubound(arr,3) /= 2) error stop "ub3"
    if (size(arr) /= 36) error stop "size"
    if (abs(arr(lb-1, lb-1, 1, 1) - 1.0d0) > 1.0d-12) error stop "v"
  end subroutine

  subroutine forward(lb, arr, pp)
    integer, intent(in) :: lb
    real(8), intent(in) :: arr(lb-1:, lb-1:, :, :)
    procedure(iface), pointer :: pp
    if (lbound(arr,1) /= lb-1) error stop "fwd lb1"
    if (ubound(arr,1) /= lb+1) error stop "fwd ub1"
    call pp(lb, arr)
  end subroutine
end module

program proc_ptr_18
  use proc_ptr_18_mod
  real(8) :: a(3, 3, 2, 2)
  procedure(iface), pointer :: pp
  a = 1.0d0
  pp => target_sub
  call forward(-4, a, pp)
  print *, "PASS"
end program
