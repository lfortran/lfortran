module main_module
   integer, dimension(2), parameter :: x = [1, 2]
end module main_module

program main
   use main_module
   implicit none
   integer(4) :: i
   i = 2
   print *, x(i)
end program main
