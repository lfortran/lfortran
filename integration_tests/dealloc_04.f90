module dealloc_04
   type, abstract :: toml_value
      integer, allocatable :: key
   end type

   type, extends(toml_value) :: toml_table
      logical :: implicit = .false.
   end type toml_table

contains

   subroutine new_table(self)
      class(toml_value), allocatable, intent(out) :: self
      allocate(self, source=toml_table())
      allocate(self%key)
      self%key = 5678
   end subroutine new_table
end module

program test
   use dealloc_04

   class(toml_value), allocatable :: tmp
   allocate(tmp, source=toml_table())
   allocate(tmp%key)
   tmp%key = 1234
   print *, "Key in tmp: ", tmp%key
   call new_table(tmp)
   print *, "Key in tmp after new_table call: ", tmp%key
   
   if (tmp%key /= 5678) error stop
end program
