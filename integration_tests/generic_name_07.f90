! Test: generic interfaces with the same name imported from multiple
! modules via `use` are merged (F2018 C1515). Here `delete` is a generic
! in both `a_m` and `b_m`; each specific dispatches on its own derived type.
module generic_name_07_a_m
  implicit none
  type :: a_t
    integer :: i = 0
  end type
  interface delete
    module procedure del_a
  end interface
contains
  subroutine del_a(x)
    type(a_t), intent(out) :: x
    x%i = -1
  end subroutine
end module

module generic_name_07_b_m
  implicit none
  type :: b_t
    integer :: j = 0
  end type
  interface delete
    module procedure del_b
  end interface
contains
  subroutine del_b(x)
    type(b_t), intent(out) :: x
    x%j = -2
  end subroutine
end module

program generic_name_07
  use generic_name_07_a_m
  use generic_name_07_b_m
  implicit none
  type(a_t) :: a
  type(b_t) :: b
  a%i = 5
  b%j = 7
  call delete(a)
  call delete(b)
  if (a%i /= -1) error stop
  if (b%j /= -2) error stop
  print *, "PASS"
end program
