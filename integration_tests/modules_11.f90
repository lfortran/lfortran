module modules_11_module11
implicit none 
integer :: i = 1
integer :: j = 2

contains      

   subroutine access_internally()          
      print*, "i = ", i
   end subroutine access_internally 
   
end module

program access_externally
use modules_11_module11
implicit none
   print*, "j = ", j
   call access_internally() 
   
end program access_externally
