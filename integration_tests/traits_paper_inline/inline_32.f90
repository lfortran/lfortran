module pairwise_library
   use traits_paper_inline_core, only: INumeric, ISum
   implicit none
   type, public :: PairwiseSum{ISum :: U}
      private
      type(U) :: other
   end type PairwiseSum

   implements ISum :: PairwiseSum{ISum :: U}
      initial :: init
      procedure, pass :: sum
   end implements PairwiseSum

contains

   function init(other) result(res)
      type(U), intent(in)  :: other
      type(PairwiseSum{U}) :: res
      res%other = other
   end function init

   function sum{INumeric :: T}(self,x) result(s)
      class(PairwiseSum{U}), intent(in) :: self
      type(T),               intent(in) :: x(:)
      type(T)                           :: s
      if (size(x) <= 2) then
         s = self%other%sum(x)
      else
         m := size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function sum

end module pairwise_library
