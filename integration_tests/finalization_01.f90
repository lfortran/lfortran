! Test finlization of variables with the same name in different modules
! We should build 2 different finalization functions.
module finalization_01_mod_1
type :: b
      character(10) :: str
      integer :: i
end type


end module

module finalization_01_mod_2
      type :: b
            character(10) :: str
      end type

      contains 
      subroutine ss()
            type(b) :: x
      end subroutine
end module 


program finalization_01
      use finalization_01_mod_1
      use finalization_01_mod_2, only : ss
      type (b) :: x
      call ss()
end program 