module gFTL2_Integer32Complex32Map
   implicit none

   type :: map_SetIterator
      integer::i
   end type map_SetIterator

   interface operator(==)
      module procedure map_s_iter_equal
   end interface operator(==)
   interface operator(/=)
      module procedure map_s_iter_not_equal
   end interface operator(/=)

   contains

   logical function map_s_iter_equal(a, b) result(eq)
      type(map_SetIterator), intent(in) :: a
      type(map_SetIterator), intent(in) :: b
      eq = a%i == b%i
   end function map_s_iter_equal

   logical function map_s_iter_not_equal(a, b) result(ne)
      implicit none
      class(map_SetIterator), intent(in) :: a, b
      ne = a == b
      ne = .not. ne
   end function map_s_iter_not_equal
end module gFTL2_Integer32Complex32Map

program polymorphic_class_compare
  use gFTL2_Integer32Complex32Map
  implicit none

  type(map_SetIterator) :: iter1, iter2

   ! Initialize iterators
   iter1%i = 1
   iter2%i = 1

  if (iter1 /= iter2) error stop "Error: iter1 and iter2 should be equal"

end program polymorphic_class_compare