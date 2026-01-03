module class_79_mod
  type, abstract :: class_value
    character(len=:), allocatable :: value
  end type
  type, extends(class_value) :: temp_type
    character(len=:), allocatable :: dir
    class(class_value), pointer :: cwd_ptr
  end type
  
contains 

  function cast_to_keyval(ptr) result(kval)
   class(class_value), intent(in), target :: ptr
   type(temp_type), pointer :: kval
   nullify(kval)
   select type(ptr)
    type is (temp_type)
        kval => ptr
   end select
end function cast_to_keyval
end module

program class_79
  use class_79_mod
  class(temp_type), pointer :: t
  class(class_value), allocatable :: p
  allocate(temp_type :: p)
  p%value = "Hello"
  t => cast_to_keyval(p)
  if (t%value /= "Hello") error stop
end program class_79