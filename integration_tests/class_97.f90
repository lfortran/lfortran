module class_97_m
  implicit none

  type :: parent_t
     integer :: x = 0
  end type parent_t

  type, extends(parent_t) :: child_t
     character(len=:), allocatable :: name
  end type child_t

contains
  function export_child(self) result(cfg)
    class(child_t), intent(in) :: self
    type(child_t) :: cfg

    cfg = self
  end function export_child
end module

program class_97
  use class_97_m
  implicit none
  type(child_t) :: a, b

  a%name = 'hello'
  b = export_child(a)
  if (len(b%name) /= 5) error stop
  print *, len(b%name)
end program class_97
