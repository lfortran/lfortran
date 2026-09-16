! A module that declares procedures with implicit interfaces, used by
! implicit_interface_82.f90.
module implicit_interface_82_mod
  implicit none
  external :: ii82_add1
  real, external :: ii82_twice
contains
  real function ii82_drive(x)
    real, intent(inout) :: x
    call ii82_add1(x)
    ii82_drive = ii82_twice(x)
  end function
end module
