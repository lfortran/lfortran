module bindc_iso_fb_06_mod
  implicit none
  interface
    subroutine check_cfi_type(a, expected_type) bind(C)
      implicit none
      type(*) :: a(..)
      integer, value :: expected_type
    end subroutine
  end interface
contains
  subroutine wrap(a, expected_type)
    type(*), intent(inout) :: a(..)
    integer, intent(in) :: expected_type
    call check_cfi_type(a, expected_type)
  end subroutine
end module

program bindc_iso_fb_06
  use bindc_iso_fb_06_mod
  implicit none
  integer :: vi
  real :: vr
  vi = 42
  vr = 3.14
  call wrap(vi, 9)
  call wrap(vr, 27)
  print *, "PASS"
end program
