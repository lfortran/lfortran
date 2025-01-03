program derived_types_38
   type :: m
      integer :: i
   end type m

   type(m) :: tt2
   tt2%i = 1

   call f()
contains

   subroutine f()
      print *, tt2
      if (tt2%i /= 1) error stop
   end subroutine f
end program derived_types_38
