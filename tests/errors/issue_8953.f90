#define FIELDS integer :: a ; real :: b

module mod_pre_a
  implicit none
  type :: T
    FIELDS
  end type T
end module mod_pre_a

module mod_pre_b
  use mod_pre_a, only: T
  implicit none
contains
  subroutine s(x)
    type(T), intent(in) :: x
  end subroutine s
end module mod_pre_b

program main_pre_chain
  use mod_pre_b, only: s
  implicit none
  integer :: r
  call s(r)
end program main_pre_chain