module bindc_iso_fb_06_mod
  implicit none
  interface
    subroutine check_cfi_type(a, expected_type) bind(C)
      implicit none
      type(*) :: a(..)
      integer, value :: expected_type
    end subroutine
    integer function cfi_type_int32() bind(C)
    end function
    integer function cfi_type_float() bind(C)
    end function
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
  call wrap(vi, cfi_type_int32())
  call wrap(vr, cfi_type_float())
  print *, "PASS"
end program
