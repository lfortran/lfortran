! Test: parent component access on extended PDT with kind type parameters
module pdt_13_parent_m
  implicit none
  type :: parent_t(k)
    integer, kind :: k = kind(1.0)
    integer :: x = 0
  end type
end module

module pdt_13_child_m
  use pdt_13_parent_m, only: parent_t
  implicit none
  type, extends(parent_t) :: child_t(m)
    integer, kind :: m = kind(1.0)
    integer :: y = 0
  end type
end module

program pdt_13
  use pdt_13_parent_m, only: parent_t
  use pdt_13_child_m, only: child_t
  implicit none
  type(child_t) :: child

  child%x = 42
  child%y = 7
  if (child%x /= 42) error stop
  if (child%y /= 7) error stop

  print *, child%x
end program
