program class_28
   type :: toml_value
        integer :: key
   end type toml_value
   class(toml_value), pointer :: ptr
   class(toml_value), allocatable :: tmp2
   allocate(ptr)
    ptr%key = 1
   tmp2 = ptr
   select type(tmp2)
      type is(toml_value)
         if (tmp2%key /= 1) error stop
      class default
         error stop "Unexpected type"
   end select
end program