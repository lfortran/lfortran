! Test: CFI descriptor elem_len is correct when allocatable array is
! passed through type(*) assumed-rank to a bind(C) subroutine.
subroutine pass_to_c(a)
  implicit none
  interface
    integer function check_elem_len(a, expected) bind(C)
      type(*), intent(in) :: a(..)
      integer, value, intent(in) :: expected
    end function
  end interface
  type(*), intent(inout) :: a(..)
  if (check_elem_len(a, 4) /= 0) error stop "FAIL: elem_len /= 4"
end subroutine

program bindc_iso_fb_07
  implicit none
  interface
    subroutine pass_to_c(a)
      type(*), intent(inout) :: a(..)
    end subroutine
  end interface
  integer, allocatable :: x(:)
  allocate(x(3))
  x = [10, 20, 30]
  call pass_to_c(x)
  print *, "PASS"
end program
