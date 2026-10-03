program main

   integer,  parameter :: i = 4

   select type (obj => unlimited_polymorphic(i))
   type is (integer)
      print *, obj
      if (obj /= 4) error stop
   class default
      error stop
   end select

contains

   function unlimited_polymorphic(i) result(res)
      integer,  intent(in) :: i
      class(*), allocatable :: res
      res = i
   end function unlimited_polymorphic

end program main
