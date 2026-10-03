program derived_types_136
   implicit none
   type :: foo
      integer :: foo
   end type foo
   type(foo) :: x
   x%foo = 42
   if (x%foo /= 42) error stop
   print *, "ok"
end program derived_types_136
