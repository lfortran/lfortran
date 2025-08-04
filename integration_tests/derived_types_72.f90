program main
   implicit none

   integer,parameter :: n = 2

   type :: my_type 
      integer :: key
   end type
   type :: my_type2 
      type(my_type), allocatable :: x(:)
   end type
   type :: matrix
      integer :: elements(n)
   end type

   type(matrix) :: m
   type(my_type) :: arr(5)
   type(my_type2) :: mt2
   integer :: i
   m%elements = [1, 2]
   arr = [(my_type(i), i=1, 5)]
   if (any(arr%key /= [1,2,3,4,5])) error stop
   allocate(mt2%x(1))
   mt2%x(1) = arr(1)
   mt2%x = [mt2%x, arr]
   if (any(mt2%x%key /= [1, 1, 2, 3, 4, 5])) error stop
end program