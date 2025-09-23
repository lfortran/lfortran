program unlimited_polymorphic_intrinsic_type_allocate
   implicit none

   class(*), allocatable :: var

   integer(8) :: x
   x = 10
   allocate(var, source=x)

   select type(var)
   type is (integer(8))
      print *, "integer(8)"
    type is (integer(4))
      print *, "integer(4)"
    type is (real)
      print *, "real"
    class default
      print *, "default"
   end select

end program unlimited_polymorphic_intrinsic_type_allocate
