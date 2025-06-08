program separate_compilation_02
   use separate_compilation_02_module
   implicit none
   integer :: i

   interface
      subroutine sub
      end subroutine sub

      integer function fn()
      end function fn
   end interface

   call sub
   call m_sub
   i = fn()
   print *, i
   if ( i /= 19 ) error stop
end program separate_compilation_02
