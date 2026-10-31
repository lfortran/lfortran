! The maintainer's interoperability variant of the paper's mixed.f90 in which the sealed PairwiseSum extends SimpleSum and overrides its
! generic sum (TD-44),
! with both averaging methods run in turn instead of read from input.
module interfaces

   use, intrinsic :: iso_fortran_env, only: real64

   implicit none
   private

   public :: INumeric, ISum, IAverager

   abstract interface :: INumeric
      integer | real(real64)
   end interface INumeric

   abstract interface :: ISum
      function sum{INumeric :: T}(self,x) result(s)
         type(itself), intent(in), pass :: self
         type(T),      intent(in) :: x(:)
         type(T)                  :: s
      end function sum
   end interface ISum

   abstract interface :: IAverager
      function average{INumeric :: T}(self,x) result(a)
         type(itself), intent(in), pass :: self
         type(T),      intent(in)       :: x(:)
         type(T)                        :: a
      end function average
   end interface IAverager

end module interfaces

module simple_library

   use interfaces, only: ISum, INumeric

   implicit none
   private

   public :: SimpleSum

   type, implements(ISum) :: SimpleSum
   contains
      procedure :: sum
      procedure :: basic_sum
   end type SimpleSum

contains

   function sum{INumeric :: T}(self,x) result(s)
      class(SimpleSum), intent(in) :: self
      type(T),          intent(in) :: x(:)
      type(T)                      :: s
      s = self%basic_sum(x)
   end function sum

   function basic_sum{INumeric :: T}(self,x) result(s)
      class(SimpleSum), intent(in) :: self
      type(T),          intent(in) :: x(:)
      type(T)                      :: s
      s = T(0)
      do i := 1, size(x)
         s = s + x(i)
      end do
   end function basic_sum

end module simple_library

module pairwise_library

   use interfaces,     only: ISum, INumeric
   use simple_library, only: SimpleSum

   implicit none
   private

   public :: PairwiseSum

   type, sealed, extends(SimpleSum), implements(ISum) :: PairwiseSum
   contains
      procedure :: sum
   end type PairwiseSum

contains

   function sum{INumeric :: T}(self,x) result(s)
      type(PairwiseSum), intent(in) :: self
      type(T),           intent(in) :: x(:)
      type(T)                       :: s
      if (size(x) <= 2) then
         s = self%basic_sum(x)
      else
         m := size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function sum

end module pairwise_library

module averager_library

   use interfaces, only: IAverager, ISum, INumeric

   implicit none
   private

   public :: Averager

   type, sealed, implements(IAverager) :: Averager
      private
      class(ISum), allocatable :: drv
   contains
      initial :: init
      procedure, pass :: average
   end type Averager

contains

   function init(drv) result(res)
      class(ISum), intent(in) :: drv
      type(Averager)          :: res
      res%drv = drv
   end function init

   function average{INumeric :: T}(self,x) result(a)
      type(Averager), intent(in) :: self
      type(T),        intent(in) :: x(:)
      type(T)                    :: a
      a = self%drv%sum(x) / T(size(x))
   end function average

end module averager_library

program main

   ! dependencies on abstractions
   use interfaces,       only: IAverager

   ! dependencies on implementations
   use simple_library,   only: SimpleSum
   use pairwise_library, only: PairwiseSum
   use averager_library, only: Averager

   implicit none

   ! declarations
   class(IAverager), allocatable :: av
   integer :: key

   xi := [1, 2, 3, 4, 5]
   xf := [1.d0, 2.d0, 3.d0, 4.d0, 5.d0]

   do key = 1, 2
      select case (key)
      case (1)
         av = Averager(drv = SimpleSum())
      case (2)
         av = Averager(drv = PairwiseSum())
      case default
         stop 'Case not implemented!'
      end select

      if (av%average(xi) /= 3) error stop 1
      if (abs(av%average(xf) - 3.d0) > 1.d-12) error stop 2
      print '(i1)',   av%average(xi)
      print '(f3.1)', av%average(xf)
      deallocate(av)
   end do

end program main
