! LFortran traits extension test; GFortran does not accept this syntax.
! The dynamic averaging structure of the paper's mixed.f90, reduced to its
! numeric-runtime obligations and extended with more data: trait-valued
! allocatable components, initializer bindings, nested pairwise forwarding
! and a scaled leaf, over integer | real(real64). Every runtime sum/average
! call passes an assumed-shape T array (empty, odd/even, reversed and strided
! sections) and returns a scalar T; T(0) and T(size(x)) supply the casts.
! A leaf counter fixes the exact recursive dispatch structure and shows that
! selected providers are reached at run time. Owner copies keep their own
! components. Standard counterpart: traits_runtime_numeric_02_oracle.f90.
module traits_runtime_numeric_02_interfaces_m

   use, intrinsic :: iso_fortran_env, only: real64

   implicit none
   private

   public :: INumeric, ISum, IAverager

   abstract interface :: INumeric
      integer | real(real64)
   end interface INumeric

   abstract interface :: ISum
      function sum{INumeric :: T}(x) result(s)
         type(T), intent(in) :: x(:)
         type(T)             :: s
      end function sum
   end interface ISum

   abstract interface :: IAverager
      function average{INumeric :: T}(x) result(a)
         type(T), intent(in) :: x(:)
         type(T)             :: a
      end function average
   end interface IAverager

end module traits_runtime_numeric_02_interfaces_m

module traits_runtime_numeric_02_simple_m

   use traits_runtime_numeric_02_interfaces_m, only: ISum, INumeric

   implicit none
   private

   public :: SimpleSum, leaf_calls

   integer :: leaf_calls = 0

   type, sealed, implements(ISum) :: SimpleSum
   contains
      procedure, nopass :: sum
   end type SimpleSum

contains

   function sum{INumeric :: T}(x) result(s)
      type(T), intent(in) :: x(:)
      type(T)             :: s
      leaf_calls = leaf_calls + 1
      s = T(0)
      do i := 1, size(x)
         s = s + x(i)
      end do
   end function sum

end module traits_runtime_numeric_02_simple_m

module traits_runtime_numeric_02_scaled_m

   use traits_runtime_numeric_02_interfaces_m, only: ISum, INumeric

   implicit none
   private

   public :: ScaledSum

   type, sealed, implements(ISum) :: ScaledSum
      integer :: factor = 1
   contains
      procedure, pass :: sum
   end type ScaledSum

contains

   function sum{INumeric :: T}(self,x) result(s)
      type(ScaledSum), intent(in) :: self
      type(T),         intent(in) :: x(:)
      type(T)                     :: s
      s = T(0)
      do i := size(x), 1, -1
         s = s + x(i)
      end do
      s = T(self%factor) * s
   end function sum

end module traits_runtime_numeric_02_scaled_m

module traits_runtime_numeric_02_pairwise_m

   use traits_runtime_numeric_02_interfaces_m, only: ISum, INumeric

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

   function sum{INumeric :: T}(self,x) result(s)
      type(PairwiseSum), intent(in) :: self
      type(T),           intent(in) :: x(:)
      type(T)                       :: s
      if (size(x) <= 2) then
         s = self%other%sum(x)
      else
         m := size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function sum

end module traits_runtime_numeric_02_pairwise_m

module traits_runtime_numeric_02_averager_m

   use traits_runtime_numeric_02_interfaces_m, only: IAverager, ISum, INumeric

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

end module traits_runtime_numeric_02_averager_m

program traits_runtime_numeric_02
   use, intrinsic :: iso_fortran_env, only: real64
   use traits_runtime_numeric_02_interfaces_m, only: ISum, IAverager
   use traits_runtime_numeric_02_simple_m, only: SimpleSum, leaf_calls
   use traits_runtime_numeric_02_scaled_m, only: ScaledSum
   use traits_runtime_numeric_02_pairwise_m, only: PairwiseSum
   use traits_runtime_numeric_02_averager_m, only: Averager
   implicit none
   ! Leaves of the recursive split for sizes 0..9 (a leaf has size <= 2).
   integer, parameter :: leaves(0:9) = [1, 1, 1, 2, 2, 3, 4, 4, 4, 5]
   integer :: xi(9), pass, k, choice, factor, checks
   real(real64) :: xr(9)
   class(ISum), allocatable :: summer
   class(IAverager), allocatable :: av, keep, twin

   checks = 0
   xi = [3, -1, 4, 1, -5, 9, 2, -6, 5]
   xr = [1.5_real64, -0.25_real64, 2.75_real64, 0.5_real64, -3.0_real64, &
      4.25_real64, 1.0_real64, -2.5_real64, 3.125_real64]

   do pass = 1, 2
      do k = 1, 4
         choice = k
         if (pass == 2) choice = 5 - k
         select case (choice)
         case (1)
            summer = SimpleSum()
            av = Averager(drv = SimpleSum())
            factor = 1
         case (2)
            summer = PairwiseSum(other = SimpleSum())
            av = Averager(drv = PairwiseSum(other = SimpleSum()))
            factor = 1
         case (3)
            summer = PairwiseSum(other = PairwiseSum(other = SimpleSum()))
            av = Averager(drv = PairwiseSum(other = PairwiseSum(other = SimpleSum())))
            factor = 1
         case (4)
            summer = PairwiseSum(other = ScaledSum(factor = 2))
            av = Averager(drv = PairwiseSum(other = ScaledSum(factor = 2)))
            factor = 2
         end select
         call check_configuration(summer, av, factor, choice)
      end do
   end do

   keep = Averager(drv = PairwiseSum(other = ScaledSum(factor = 2)))
   twin = keep
   keep = Averager(drv = SimpleSum())
   leaf_calls = 0
   call check_i(twin%average(xi), 2)
   call check_i(leaf_calls, 0)
   call check_i(keep%average(xi), 1)
   call check_i(leaf_calls, 1)
   call check_mean(twin%average(xr), 14.75_real64 / 9.0_real64)

   deallocate(summer, av, keep, twin)
   print '(a,i0,a)', 'traits_runtime_numeric_02: ', checks, ' checks passed'

contains

   subroutine check_configuration(summer, av, factor, choice)
      class(ISum),      intent(in) :: summer
      class(IAverager), intent(in) :: av
      integer,          intent(in) :: factor, choice
      integer :: n
      do n = 0, 9
         leaf_calls = 0
         call check_i(summer%sum(xi(1:n)), factor * reference_i(xi(1:n)))
         call check_i(leaf_calls, expected_leaves(n, choice))
         leaf_calls = 0
         call check_r(summer%sum(xr(n:1:-1)), factor * reference_r(xr(n:1:-1)))
         call check_i(leaf_calls, expected_leaves(n, choice))
         if (n > 0) then
            leaf_calls = 0
            call check_i(av%average(xi(1:n)), (factor * reference_i(xi(1:n))) / n)
            call check_i(leaf_calls, expected_leaves(n, choice))
            call check_mean(av%average(xr(1:n)), &
               (factor * reference_r(xr(1:n))) / real(n, real64))
         end if
      end do
      leaf_calls = 0
      call check_i(summer%sum(xi(1:9:2)), factor * reference_i(xi(1:9:2)))
      call check_i(leaf_calls, expected_leaves(5, choice))
      call check_i(av%average(xi(9:1:-3)), (factor * 18) / 3)
      call check_mean(av%average(xr(2:9:3)), (factor * (-5.75_real64)) / 3.0_real64)
      call check_i(av%average(xi(1:4)) * 2 + summer%sum(xi(1:2)), &
         2 * ((factor * 7) / 4) + factor * 2)
      call check_r(av%average(xr(1:8)) - summer%sum(xr(1:0)), &
         (factor * 4.25_real64) / 8.0_real64)
   end subroutine check_configuration

   function expected_leaves(n, choice) result(count)
      integer, intent(in) :: n, choice
      integer :: count
      select case (choice)
      case (1)
         count = 1
      case (2, 3)
         count = leaves(n)
      case default
         count = 0
      end select
   end function expected_leaves

   function reference_i(x) result(s)
      integer, intent(in) :: x(:)
      integer :: s, i
      s = 0
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function reference_i

   function reference_r(x) result(s)
      real(real64), intent(in) :: x(:)
      real(real64) :: s
      integer :: i
      s = 0.0_real64
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function reference_r

   subroutine check_i(actual, expected)
      integer, intent(in) :: actual, expected
      checks = checks + 1
      if (actual /= expected) then
         print '(a,i0,a,i0,a,i0)', 'check ', checks, ': ', actual, ' /= ', expected
         error stop 1
      end if
   end subroutine check_i

   ! Dyadic data make every summation order exact.
   subroutine check_r(actual, expected)
      real(real64), intent(in) :: actual, expected
      checks = checks + 1
      if (actual /= expected) then
         print '(a,i0,a,es24.16,a,es24.16)', 'check ', checks, ': ', actual, &
            ' /= ', expected
         error stop 2
      end if
   end subroutine check_r

   ! An average divides an exact sum once; optimized division may round
   ! through a reciprocal, so allow a few units in the last place.
   subroutine check_mean(actual, expected)
      real(real64), intent(in) :: actual, expected
      checks = checks + 1
      if (.not. (abs(actual - expected) <= 4 * spacing(abs(expected)))) then
         print '(a,i0,a,es24.16,a,es24.16)', 'check ', checks, ': ', actual, &
            ' !~ ', expected
         error stop 3
      end if
   end subroutine check_mean
end program traits_runtime_numeric_02
