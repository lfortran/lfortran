module traits_invalid_implements_01
   implicit none

   type :: Concrete
      integer :: value
   end type Concrete

   type, implements(Concrete) :: Bad
      integer :: value
   end type Bad

end module traits_invalid_implements_01
