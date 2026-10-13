! Standard Fortran counterpart of traits_runtime_numeric_04.f90: one deferred
! binding per member of integer | real | complex replaces each generic message,
! and polymorphic components replace the generic type parameters. It performs
! the same 22 checks.
module traits_runtime_numeric_04_oracle_m
   implicit none
   private
   public :: ISum, IAverager, SimpleSum, PairwiseSum, Averager

   type, abstract :: ISum
   contains
      procedure(sum_i), deferred :: sum_i
      procedure(sum_r), deferred :: sum_r
      procedure(sum_c), deferred :: sum_c
      generic :: sum => sum_i, sum_r, sum_c
   end type ISum

   abstract interface
      function sum_i(self, x) result(s)
         import :: ISum
         class(ISum), intent(in) :: self
         integer,     intent(in) :: x(:)
         integer                 :: s
      end function sum_i
      function sum_r(self, x) result(s)
         import :: ISum
         class(ISum), intent(in) :: self
         real,        intent(in) :: x(:)
         real                    :: s
      end function sum_r
      function sum_c(self, x) result(s)
         import :: ISum
         class(ISum), intent(in) :: self
         complex,     intent(in) :: x(:)
         complex                 :: s
      end function sum_c
   end interface

   type, abstract :: IAverager
   contains
      procedure(average_i), deferred :: average_i
      procedure(average_r), deferred :: average_r
      procedure(average_c), deferred :: average_c
      generic :: average => average_i, average_r, average_c
   end type IAverager

   abstract interface
      function average_i(self, x) result(a)
         import :: IAverager
         class(IAverager), intent(in) :: self
         integer,          intent(in) :: x(:)
         integer                      :: a
      end function average_i
      function average_r(self, x) result(a)
         import :: IAverager
         class(IAverager), intent(in) :: self
         real,             intent(in) :: x(:)
         real                         :: a
      end function average_r
      function average_c(self, x) result(a)
         import :: IAverager
         class(IAverager), intent(in) :: self
         complex,          intent(in) :: x(:)
         complex                      :: a
      end function average_c
   end interface

   type, extends(ISum) :: SimpleSum
   contains
      procedure :: sum_i => simple_i
      procedure :: sum_r => simple_r
      procedure :: sum_c => simple_c
   end type SimpleSum

   type, extends(ISum) :: PairwiseSum
      class(ISum), allocatable :: other
   contains
      procedure :: sum_i => pairwise_i
      procedure :: sum_r => pairwise_r
      procedure :: sum_c => pairwise_c
   end type PairwiseSum

   type, extends(IAverager) :: Averager
      class(ISum), allocatable :: drv
   contains
      procedure :: average_i => averager_i
      procedure :: average_r => averager_r
      procedure :: average_c => averager_c
   end type Averager

contains

   function simple_i(self, x) result(s)
      class(SimpleSum), intent(in) :: self
      integer,          intent(in) :: x(:)
      integer                      :: s
      integer                      :: i
      s = 0
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function simple_i

   function simple_r(self, x) result(s)
      class(SimpleSum), intent(in) :: self
      real,             intent(in) :: x(:)
      real                         :: s
      integer                      :: i
      s = 0.0
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function simple_r

   function simple_c(self, x) result(s)
      class(SimpleSum), intent(in) :: self
      complex,          intent(in) :: x(:)
      complex                      :: s
      integer                      :: i
      s = (0.0, 0.0)
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function simple_c

   recursive function pairwise_i(self, x) result(s)
      class(PairwiseSum), intent(in) :: self
      integer,            intent(in) :: x(:)
      integer                        :: s
      integer                        :: m
      if (size(x) <= 2) then
         s = self%other%sum(x)
      else
         m = size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function pairwise_i

   recursive function pairwise_r(self, x) result(s)
      class(PairwiseSum), intent(in) :: self
      real,               intent(in) :: x(:)
      real                           :: s
      integer                        :: m
      if (size(x) <= 2) then
         s = self%other%sum(x)
      else
         m = size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function pairwise_r

   recursive function pairwise_c(self, x) result(s)
      class(PairwiseSum), intent(in) :: self
      complex,            intent(in) :: x(:)
      complex                        :: s
      integer                        :: m
      if (size(x) <= 2) then
         s = self%other%sum(x)
      else
         m = size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function pairwise_c

   function averager_i(self, x) result(a)
      class(Averager), intent(in) :: self
      integer,         intent(in) :: x(:)
      integer                     :: a
      a = self%drv%sum(x) / size(x)
   end function averager_i

   function averager_r(self, x) result(a)
      class(Averager), intent(in) :: self
      real,            intent(in) :: x(:)
      real                        :: a
      a = self%drv%sum(x) / real(size(x))
   end function averager_r

   function averager_c(self, x) result(a)
      class(Averager), intent(in) :: self
      complex,         intent(in) :: x(:)
      complex                     :: a
      a = self%drv%sum(x) / cmplx(size(x), 0)
   end function averager_c

end module traits_runtime_numeric_04_oracle_m

program traits_runtime_numeric_04_oracle
   use traits_runtime_numeric_04_oracle_m, only: IAverager, SimpleSum, PairwiseSum, Averager
   implicit none
   integer :: checks

   checks = 0
   call run(1)
   call run(2)
   if (checks /= 22) error stop 99
   print '(a,i0,a)', 'traits_runtime_numeric_04_oracle: ', checks, ' checks passed'

contains

   subroutine run(key)
      integer, intent(in) :: key
      class(IAverager), allocatable :: av
      integer :: xi(5)
      real    :: xr(5)
      complex :: xc(5), yc(4)

      select case (key)
      case (1)
         allocate(av, source=Averager(drv=SimpleSum()))
      case (2)
         allocate(av, source=Averager(drv=PairwiseSum(other=SimpleSum())))
      case default
         error stop 'Invalid averaging method!'
      end select

      xi = [1, 2, 3, 4, 5]
      xr = [1.0, 2.0, 3.0, 4.0, 5.0]
      xc = [(1.0,1.0), (2.0,2.0), (3.0,3.0), (4.0,4.0), (5.0,5.0)]
      yc = [(1.0,-2.0), (3.0,4.0), (5.0,0.0), (7.0,2.0)]

      call check(av%average(xi) == 3)
      call check(av%average(xr) == 3.0)
      call check(av%average(xc) == (3.0,3.0))
      call check(kind(av%average(xc)) == kind(xc))
      call check(av%average(xc(5:1:-1)) == (3.0,3.0))
      call check(av%average(xc(1:5:2)) == (3.0,3.0))
      call check(av%average(xc(4:4)) == (4.0,4.0))
      call check(av%average(yc) == (4.0,1.0))
      call check(av%average(yc(4:1:-3)) == (4.0,0.0))
      call check(av%average(yc(2:3)) == (4.0,2.0))
      call check(av%average(xi(2:4)) == 3)
   end subroutine run

   subroutine check(condition)
      logical, intent(in) :: condition
      checks = checks + 1
      if (.not. condition) then
         print '(a,i0,a)', 'check ', checks, ' failed'
         error stop 1
      end if
   end subroutine check

end program traits_runtime_numeric_04_oracle
