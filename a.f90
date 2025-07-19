program main
   implicit none

   integer :: n = 2

   type :: matrix
      integer :: elements(n)
   end type

   type(matrix) :: m
   m%elements = [1, 2]
end program