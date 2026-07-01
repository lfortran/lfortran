! Create 2 modules for 2 nestedvar containing functions (i.e. `a`)
! Make sure no symboltable conflicts
module nested_26_mod_01
   contains
   subroutine a()
      integer :: i
      
      contains

      subroutine b()
         print *, i
      end subroutine

   end subroutine

end module 

module nested_26_mod_02
   contains
   subroutine a()
      integer :: i
      
      contains

      subroutine b()
         print *, i
      end subroutine

   end subroutine

end module 


program nested_26
   use nested_26_mod_01
   call a()
   contains 
   subroutine ss1
      use nested_26_mod_02
      call a()
   end subroutine
end program 