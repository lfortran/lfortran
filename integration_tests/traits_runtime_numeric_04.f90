! LFortran traits extension test; GFortran does not accept this syntax.
! A user's averaging program over the type set integer | real | complex: the
! generic messages of IAverager return the member type, so the complex member
! slot returns a complex scalar through the class(IAverager) view. Both
! averaging methods, simple and recursive pairwise summation, are checked
! instead of reading the choice; traits_runtime_numeric_04_oracle.f90 is a
! standard Fortran counterpart with the same checks.
module traits_runtime_numeric_04_interfaces

   implicit none
   private

   public :: INumeric, ISum, IAverager

   abstract interface :: INumeric
      integer | real | complex
   end interface INumeric

   abstract interface :: ISum
      function sum{INumeric :: T}(self, x) result(s)
         type(itself), intent(in), pass :: self
         type(T), intent(in) :: x(:)
         type(T)             :: s
      end function sum
   end interface ISum

   abstract interface :: IAverager
      function average{INumeric :: T}(self, x) result(a)
         type(itself), intent(in), pass :: self
         type(T), intent(in) :: x(:)
         type(T)             :: a
      end function average
   end interface IAverager

end module traits_runtime_numeric_04_interfaces

module traits_runtime_numeric_04_simple

   use traits_runtime_numeric_04_interfaces, only: ISum, INumeric

   implicit none
   private

   public :: SimpleSum

   type, sealed, implements(ISum) :: SimpleSum
   contains
      procedure, pass :: sum
   end type SimpleSum

contains

   function sum{INumeric :: T}(self, x) result(s)
      type(SimpleSum), intent(in) :: self
      type(T), intent(in) :: x(:)
      type(T)             :: s
      s = T(0)
      do i := 1, size(x)
         s = s + x(i)
      end do
   end function sum

end module traits_runtime_numeric_04_simple

module traits_runtime_numeric_04_pairwise

   use traits_runtime_numeric_04_interfaces, only: ISum, INumeric

   implicit none
   private

   public :: PairwiseSum

   type, sealed, implements(ISum) :: PairwiseSum{ISum :: U}
      private
      type(U), allocatable :: other
   contains
      initial :: init
      procedure, pass :: sum
   end type PairwiseSum

contains

   function init(other) result(res)
      type(U), intent(in)  :: other
      type(PairwiseSum{U}) :: res
      res%other = other
   end function init

   function sum{INumeric :: T}(self,x) result(s)
      type(PairwiseSum{U}), intent(in) :: self
      type(T),              intent(in) :: x(:)
      type(T)                          :: s
      if (size(x) <= 2) then
         s = self%other%sum(x)
      else
         m := size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function sum

end module traits_runtime_numeric_04_pairwise

module traits_runtime_numeric_04_averager

   use traits_runtime_numeric_04_interfaces, only: IAverager, ISum, INumeric

   implicit none
   private

   public :: Averager

   type, sealed, implements(IAverager) :: Averager{ISum :: U}
      private
      type(U), allocatable :: drv
   contains
      initial :: init
      procedure, pass :: average
   end type Averager

contains

   function init(drv) result(res)
      type(U), intent(in) :: drv
      type(Averager{U})   :: res
      res%drv = drv
   end function init

   function average{INumeric :: T}(self,x) result(a)
      type(Averager{U}), intent(in) :: self
      type(T),           intent(in) :: x(:)
      type(T)                       :: a
      a = self%drv%sum(x) / T(size(x))
   end function average

end module traits_runtime_numeric_04_averager

program traits_runtime_numeric_04

   use traits_runtime_numeric_04_interfaces, only: IAverager
   use traits_runtime_numeric_04_simple,     only: SimpleSum
   use traits_runtime_numeric_04_pairwise,   only: PairwiseSum
   use traits_runtime_numeric_04_averager,   only: Averager

   implicit none

   integer :: checks
   ! --fast may approximate the divisions by reciprocal estimates (LLVM 7/8
   ! vectorized x86 code is one ulp off), so floating averages use a tolerance.
   real, parameter :: tol = 1e-5

   checks = 0
   call run(1)
   call run(2)
   if (checks /= 22) error stop 99
   print '(a,i0,a)', 'traits_runtime_numeric_04: ', checks, ' checks passed'

contains

   subroutine run(key)
      integer, intent(in) :: key
      class(IAverager), allocatable :: av

      select case (key)
      case (1)
         av = Averager(drv = SimpleSum())
      case (2)
         av = Averager(drv = PairwiseSum(other = SimpleSum()))
      case default
         error stop 'Invalid averaging method!'
      end select

      xi := [1, 2, 3, 4, 5]
      xr := [1.0, 2.0, 3.0, 4.0, 5.0]
      xc := [(1.0,1.0), (2.0,2.0), (3.0,3.0), (4.0,4.0), (5.0,5.0)]
      yc := [(1.0,-2.0), (3.0,4.0), (5.0,0.0), (7.0,2.0)]

      call check(av%average(xi) == 3)
      call check(abs(av%average(xr) - 3.0) < tol)
      call check(abs(av%average(xc) - (3.0,3.0)) < tol)
      call check(kind(av%average(xc)) == kind(xc))
      ! Reversed, strided and length-one sections of the complex member.
      call check(abs(av%average(xc(5:1:-1)) - (3.0,3.0)) < tol)
      call check(abs(av%average(xc(1:5:2)) - (3.0,3.0)) < tol)
      call check(abs(av%average(xc(4:4)) - (4.0,4.0)) < tol)
      call check(abs(av%average(yc) - (4.0,1.0)) < tol)
      call check(abs(av%average(yc(4:1:-3)) - (4.0,0.0)) < tol)
      call check(abs(av%average{complex}(yc(2:3)) - (4.0,2.0)) < tol)
      call check(av%average{integer}(xi(2:4)) == 3)
   end subroutine run

   subroutine check(condition)
      logical, intent(in) :: condition
      checks = checks + 1
      if (.not. condition) then
         print '(a,i0,a)', 'check ', checks, ' failed'
         error stop 1
      end if
   end subroutine check

end program traits_runtime_numeric_04
