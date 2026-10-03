module separate_compilation_37b
   use separate_compilation_37a, only: AbsType1
   implicit none

   type, abstract :: AbsType2
   end type AbsType2

contains

   function get_obj() result(obj)
      class(AbsType2), allocatable :: obj
      class(AbsType1), allocatable :: c
      select type( o => c%method('String') )
      class is (AbsType2)
         obj = o
      end select
   end function get_obj

end module separate_compilation_37b
