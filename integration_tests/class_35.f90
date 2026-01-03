program class_35
   implicit none
   type :: toml_value
      integer :: key
   end type toml_value

   class(toml_value), pointer :: ptr
   class(toml_value), allocatable :: tmp2
   allocate(ptr, tmp2)

   ptr%key = 1
   tmp2 = ptr

   print *, "tmp2%key: ", tmp2%key

   select type(tmp2)
    type is(toml_value)
      if (tmp2%key /= 1) error stop
    class default
      error stop "Unexpected type"
   end select

end program
