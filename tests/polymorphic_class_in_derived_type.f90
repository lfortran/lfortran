module fortuno_types
   implicit none

   type :: dict_item
      class(*), allocatable :: value
      class(*), allocatable :: value2
   end type dict_item

end module fortuno_types
