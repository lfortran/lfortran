module derived_types_49_m
    implicit none
    public :: base, derived

    type, abstract :: base
        integer :: a
    end type base

    type, extends(base) :: derived
        integer :: b
    end type derived

    type, extends(derived) :: derived2
        integer :: c
        integer :: d
    end type derived2
end module derived_types_49_m

program derived_types_49
  use derived_types_49_m
  implicit none

  type(derived2) :: set0, set1
  set0 = derived2(10, 20, 30, 40)

  set1 = set0

  if (set1%a /= set0%a) error stop
  if (set1%b /= set0%b) error stop
  if (set1%c /= set0%c) error stop
  if (set1%d /= set0%d) error stop
end program derived_types_49
