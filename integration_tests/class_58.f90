module class_58_mod
  implicit none
  type :: mytype
    integer :: val
  end type mytype
contains
  subroutine expect_class(x)
    class(mytype), intent(in) :: x(:)
    ! print *, x(1)%val   ! This can be done after StructType Refactor 
  end subroutine expect_class
end module class_58_mod


program class_58
  use class_58_mod
  implicit none
  type(mytype), allocatable :: y(:) 
  allocate(y(3))
  y(1)%val = 5
  call expect_class(y)
end program class_58