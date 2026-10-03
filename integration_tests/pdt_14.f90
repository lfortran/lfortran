! Test: deepcopy of derived type containing allocatable array of PDT with TBPs
module pdt_14_m
  implicit none
  type :: inner_t(k)
    integer, kind :: k = kind(1.)
    real(k) :: val = 0.0
  contains
    procedure :: get_val
  end type

  type :: outer_t
    type(inner_t), allocatable :: items(:)
  end type
contains
  real function get_val(self)
    class(inner_t), intent(in) :: self
    get_val = self%val
  end function
end module

program pdt_14
  use pdt_14_m
  implicit none
  type(outer_t) :: a, b

  allocate(a%items(3))
  a%items(1)%val = 1.0
  a%items(2)%val = 2.0
  a%items(3)%val = 3.0

  b = a

  if (size(b%items) /= 3) error stop
  if (abs(b%items(1)%val - 1.0) > 1.0e-6) error stop
  if (abs(b%items(2)%val - 2.0) > 1.0e-6) error stop
  if (abs(b%items(3)%val - 3.0) > 1.0e-6) error stop
  if (abs(b%items(2)%get_val() - 2.0) > 1.0e-6) error stop

  ! Verify deep copy: modifying a should not affect b
  a%items(1)%val = 99.0
  if (abs(b%items(1)%val - 1.0) > 1.0e-6) error stop

  print *, "PASS"
end program
