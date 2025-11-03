program main
   type :: type_a
      integer :: a
   end type type_a

   class(type_a), allocatable :: x

   allocate(x)
   x%a = 10

   select type(x)
    type is(type_a)
        if (x%a /= 10) error stop "FAILED: wrong value"
    class default
        error stop "FAILED: wrong type in select type"
   end select
end program
