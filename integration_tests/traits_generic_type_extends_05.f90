! PairwiseSum extends the instance SimpleSum{BasicSum} of a separately
! compiled generic type (traits_generic_type_extends_05_provider.f90).
module traits_generic_type_extends_05_pairwise

   use traits_generic_type_extends_05_interfaces, only: ISum, INumeric
   use traits_generic_type_extends_05_basic, only: BasicSum
   use traits_generic_type_extends_05_provider, only: SimpleSum

   implicit none
   private

   public :: PairwiseSum
   
   type, extends(SimpleSum{BasicSum}) :: PairwiseSum
   contains
      procedure :: sum
   end type PairwiseSum

contains

   function sum{INumeric :: T}(self,x) result(s)
      class(PairwiseSum), intent(in) :: self
      type(T),            intent(in) :: x(:)
      type(T)                        :: s
      if (size(x) <= 2) then
         s = self%basic%sum(x)
      else
         m := size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function sum

end module traits_generic_type_extends_05_pairwise


module traits_generic_type_extends_05_averager

   use traits_generic_type_extends_05_interfaces, only: IAverager, ISum, INumeric

   implicit none
   private

   public :: Averager

   type, implements(IAverager) :: Averager{ISum :: U}
      private
      type(U), allocatable :: drv
   contains
      initial :: init
      procedure :: average
   end type Averager

contains

   function init(drv) result(res)
      type(U), intent(in) :: drv
      type(Averager{U})   :: res
      res%drv = drv
   end function init
   
   function average{INumeric :: T}(self,x) result(a)
      class(Averager{U}), intent(in) :: self
      type(T),            intent(in) :: x(:)
      type(T)                        :: a
      a = self%drv%sum(x) / T(size(x))
   end function average

end module traits_generic_type_extends_05_averager

program traits_generic_type_extends_05

   ! dependencies on abstractions
   use traits_generic_type_extends_05_interfaces, only: IAverager

   ! dependencies on implementations
   use traits_generic_type_extends_05_basic, only: BasicSum
   use traits_generic_type_extends_05_provider, only: SimpleSum
   use traits_generic_type_extends_05_pairwise, only: PairwiseSum
   use traits_generic_type_extends_05_averager, only: Averager

   implicit none
   
   ! declarations
   class(IAverager), allocatable :: av
   integer :: key

   xi := [1, 2, 3, 4, 5]
   xf := [1.d0, 2.d0, 3.d0, 4.d0, 5.d0]

   do key = 1, 2
      select case (key)
      case (1)
         av = Averager(drv = SimpleSum(BasicSum()))
      case (2)
         av = Averager(drv = PairwiseSum(BasicSum()))
      case default
         stop 'Case not implemented!'
      end select

      if (av%average(xi) /= 3) error stop 1
      if (abs(av%average(xf) - 3.d0) > 1.d-12) error stop 2
      print '(i1)',   av%average(xi)
      print '(f3.1)', av%average(xf)
      deallocate(av)
   end do

end program traits_generic_type_extends_05
