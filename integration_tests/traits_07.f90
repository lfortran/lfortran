module traits_07_interfaces

   use, intrinsic :: iso_fortran_env, only: real64

   implicit none
   private

   public :: ISum, IAverager

   abstract interface :: ISum
      function sum(x) result(s)
         import :: real64
         real(real64), intent(in) :: x(:)
         real(real64)             :: s
      end function sum
   end interface ISum

   abstract interface :: IAverager
      function average(x) result(a)
         import :: real64
         real(real64), intent(in) :: x(:)
         real(real64)             :: a
      end function average
   end interface IAverager

end module traits_07_interfaces

module traits_07_simple_library

   use, intrinsic :: iso_fortran_env, only: real64
   use traits_07_interfaces, only: ISum

   implicit none
   private

   public :: SimpleSum

   type, sealed, implements(ISum) :: SimpleSum
   contains
      procedure, nopass :: sum
   end type SimpleSum

contains

   function sum(x) result(s)
      real(real64), intent(in) :: x(:)
      real(real64)             :: s
      integer :: i
      s = real(0,kind=real64)
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function sum

end module traits_07_simple_library

module traits_07_pairwise_library

   use, intrinsic :: iso_fortran_env, only: real64
   use traits_07_interfaces, only: ISum

   implicit none
   private

   public :: PairwiseSum

   type, sealed, implements(ISum) :: PairwiseSum
      private
      class(ISum), allocatable :: other
   contains
      initial :: init
      procedure, pass :: sum
   end type PairwiseSum

contains

   function init(other) result(res)
      class(ISum), intent(in) :: other
      type(PairwiseSum)       :: res
      res%other = other
   end function init

   function sum(self,x) result(s)
      type(PairwiseSum), intent(in) :: self
      real(real64),      intent(in) :: x(:)
      real(real64)                  :: s
      integer :: m
      if (size(x) <= 2) then
         s = self%other%sum(x)
      else
         m = size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function sum

end module traits_07_pairwise_library

module traits_07_averager_library

   use, intrinsic :: iso_fortran_env, only: real64
   use traits_07_interfaces, only: IAverager, ISum

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

   function average(self,x) result(a)
      type(Averager), intent(in) :: self
      real(real64),   intent(in) :: x(:)
      real(real64)               :: a
      a = self%drv%sum(x) / real(size(x),kind=real64)
   end function average

end module traits_07_averager_library

program traits_07

   use traits_07_interfaces,       only: IAverager
   use traits_07_simple_library,   only: SimpleSum
   use traits_07_pairwise_library, only: PairwiseSum
   use traits_07_averager_library, only: Averager

   implicit none

   class(IAverager), allocatable :: av

   av = Averager(drv = SimpleSum())
   if (abs(av%average([1.d0, 2.d0, 3.d0, 4.d0, 5.d0]) - 3.d0) > 1e-12) error stop 1

   av = Averager(drv = PairwiseSum(other = SimpleSum()))
   if (abs(av%average([1.d0, 2.d0, 3.d0, 4.d0, 5.d0]) - 3.d0) > 1e-12) error stop 2

   print *, "PASS"

end program traits_07
