module class_51_mod

   type :: MyType
      integer :: value
   end type MyType

contains

   function my_class_func() result(obj)
      class(MyType), allocatable :: obj
      allocate(obj)
      obj % value = 42
   end function

   function my_type_func() result(obj)
      type(MyType), allocatable :: obj
      allocate(obj)
      obj % value = 37
   end function

   function my_class_func_ptr() result(obj)
      class(MyType), pointer :: obj
      allocate(obj)
      obj % value = 42
   end function

   function my_type_func_ptr() result(obj)
      type(MyType), pointer :: obj
      allocate(obj)
      obj % value = 37
   end function

end module class_51_mod

program class_51
   use class_51_mod
   implicit none

   class(MyType), allocatable :: my_class_var
   class(MyType), pointer :: my_class_var_ptr
   type(MyType), allocatable :: my_type_var
   type(MyType), pointer :: my_type_var_ptr

   my_class_var = my_class_func()

   print *, "my_class_var%value: ", my_class_var%value
   if (my_class_var%value /= 42) error stop

   my_type_var = my_type_func()

   print *, "my_type_var%value: ", my_type_var%value
   if (my_type_var%value /= 37) error stop

   my_class_var_ptr => my_class_func_ptr()

   print *, "my_class_var_ptr%value: ", my_class_var_ptr%value
   if (my_class_var_ptr%value /= 42) error stop

   my_type_var_ptr => my_type_func_ptr()

   print *, "my_type_var_ptr%value: ", my_type_var_ptr%value
   if (my_type_var_ptr%value /= 37) error stop

end program