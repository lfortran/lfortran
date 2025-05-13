module gFTL2_Integer32Complex32Map
   implicit none

   type :: map_SetIterator
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
      eq = .true.
   end function map_s_iter_equal

   logical function map_s_iter_not_equal(a, b) result(ne)
      implicit none
      class(map_SetIterator), intent(in) :: a, b
      ne = a == b
   end function map_s_iter_not_equal
end module gFTL2_Integer32Complex32Map