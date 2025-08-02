program main
   implicit none

   integer,parameter :: n = 2

   type :: my_type 
      integer :: key
   end type
   type :: matrix
      integer :: elements(n)
   end type

   type(matrix) :: m
   type(my_type) :: arr(5)
   integer :: i
   m%elements = [1, 2]
   arr = [(my_type(i), i=1, 5)]
   if (any(arr%key /= [1,2,3,4,5])) error stop
end program