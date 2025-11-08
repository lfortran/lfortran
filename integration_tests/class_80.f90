! Nested CLASS dummy call capturing host-associated self should not segfault
module module_class_80
   implicit none
   type, abstract :: abstract_lexer
   end type abstract_lexer
   type, extends(abstract_lexer) :: mocked_lexer
        integer, allocatable :: token(:)
   end type mocked_lexer
contains
   subroutine xx(lexer, val)
      class(abstract_lexer), intent(inout) :: lexer
      integer, intent(inout) :: val
      call nested1(val)

      contains
         subroutine nested1(val)
            integer, intent(inout) :: val
            val = val + 1
            call nested2(val)
         contains
            subroutine nested2(val)
               integer, intent(inout) :: val
               val = val + 2
            end subroutine nested2
         end subroutine nested1
   end subroutine xx

   subroutine semantic_error_tmp(lexer)
      class(abstract_lexer), intent(inout) :: lexer
   end subroutine semantic_error_tmp
end module module_class_80

program class_80
   use module_class_80
   implicit none
   type(mocked_lexer) :: lexer
   integer :: i

   i = 0
   call xx(lexer, i)
   if (i /= 3) then
      error stop "class_80: wrong value after nested call"
   end if
end program class_80
