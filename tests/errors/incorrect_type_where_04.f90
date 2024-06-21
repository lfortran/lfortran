module main_module
   implicit none
   character(5), parameter :: c = "hello"
end module main_module

program main
   use main_module
   implicit none
   integer  :: b(5)

   where(c) b = 12121
   print *, b
end program main
