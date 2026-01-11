module class_80_mod
   type :: class_value
    character(len=:), allocatable :: value
  end type
  type :: temp_type
    character(len=:), allocatable :: value
  end type
contains 
  subroutine cast_to_keyval(ptr, val)
   class(class_value), intent(in) :: ptr(:)
   type(temp_type), intent(in), optional :: val(:)
!    val(1)%value = ptr(1)%value   ! TODO 
end subroutine cast_to_keyval 
end module

program class_80
  use class_80_mod
  type(temp_type), allocatable :: x(:)
  type(class_value), allocatable :: y(:)
  allocate(x(1))
  allocate(y(1))
  y(1)%value = "Hello World"
  call cast_to_keyval(y, x)
!   if (x(1)%value /= "Hello World") error stop  ! TODO
end program class_80