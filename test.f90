program main
   type :: type_a
      integer :: a
   end type type_a

   class(type_a), allocatable :: x

   allocate(x)
   x%a = 10

   select type(x)
    type is (type_a)
      print *, x
    class default
      print *, "default"
   end select
end program