! Standard-Fortran oracle for the paper's Code/Fortran/static.f90 (sha256
! 7c07edea1d8a5c75b39e682f864e814f1e106ff7bfd31baf45ff0abaf560dfbf). The
! instances the paper's main program creates are concretized by hand:
! PairwiseSum{SimpleSum}, Averager{SimpleSum} and
! Averager{PairwiseSum{SimpleSum}}; T-generic methods become generic bindings
! over integer and real(real64); IAverager becomes an abstract type with
! deferred integer and real(real64) bindings; initializers are generic
! interfaces named like their types; := declarations are explicit. The
! prompts, READ, SELECT CASE and output formats are unchanged.
module interfaces

   use, intrinsic :: iso_fortran_env, only: real64

   implicit none
   private

   public :: IAverager

   type, abstract :: IAverager
   contains
      procedure(average_i_iface), deferred, pass :: average_i
      procedure(average_r_iface), deferred, pass :: average_r
      generic :: average => average_i, average_r
   end type IAverager

   abstract interface
      function average_i_iface(self, x) result(a)
         import :: IAverager
         class(IAverager), intent(in) :: self
         integer,          intent(in) :: x(:)
         integer                      :: a
      end function average_i_iface
      function average_r_iface(self, x) result(a)
         import :: IAverager, real64
         class(IAverager), intent(in) :: self
         real(real64),     intent(in) :: x(:)
         real(real64)                 :: a
      end function average_r_iface
   end interface

end module interfaces

module simple_library

   use, intrinsic :: iso_fortran_env, only: real64

   implicit none
   private

   public :: SimpleSum

   type :: SimpleSum
   contains
      procedure, nopass :: sum_i
      procedure, nopass :: sum_r
      generic :: sum => sum_i, sum_r
   end type SimpleSum

contains

   function sum_i(x) result(s)
      integer, intent(in) :: x(:)
      integer             :: s
      integer             :: i
      s = 0
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function sum_i

   function sum_r(x) result(s)
      real(real64), intent(in) :: x(:)
      real(real64)             :: s
      integer                  :: i
      s = 0
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function sum_r

end module simple_library

! The instances that static.f90 creates in its main program.
module static_instances

   use, intrinsic :: iso_fortran_env, only: real64
   use interfaces,     only: IAverager
   use simple_library, only: SimpleSum

   implicit none
   private

   public :: PairwiseSum_SimpleSum, Averager_SimpleSum
   public :: Averager_PairwiseSum_SimpleSum

   type :: PairwiseSum_SimpleSum
      private
      type(SimpleSum), allocatable :: other
   contains
      procedure, pass :: sum_i => pairwise_sum_i
      procedure, pass :: sum_r => pairwise_sum_r
      generic :: sum => sum_i, sum_r
   end type PairwiseSum_SimpleSum
   interface PairwiseSum_SimpleSum
      module procedure pairwise_init
   end interface PairwiseSum_SimpleSum

   type, extends(IAverager) :: Averager_SimpleSum
      private
      type(SimpleSum), allocatable :: drv
   contains
      procedure, pass :: average_i => simple_average_i
      procedure, pass :: average_r => simple_average_r
   end type Averager_SimpleSum
   interface Averager_SimpleSum
      module procedure simple_averager_init
   end interface Averager_SimpleSum

   type, extends(IAverager) :: Averager_PairwiseSum_SimpleSum
      private
      type(PairwiseSum_SimpleSum), allocatable :: drv
   contains
      procedure, pass :: average_i => pairwise_average_i
      procedure, pass :: average_r => pairwise_average_r
   end type Averager_PairwiseSum_SimpleSum
   interface Averager_PairwiseSum_SimpleSum
      module procedure pairwise_averager_init
   end interface Averager_PairwiseSum_SimpleSum

contains

   function pairwise_init(other) result(res)
      type(SimpleSum), intent(in) :: other
      type(PairwiseSum_SimpleSum) :: res
      res%other = other
   end function pairwise_init

   recursive function pairwise_sum_i(self, x) result(s)
      class(PairwiseSum_SimpleSum), intent(in) :: self
      integer,                      intent(in) :: x(:)
      integer                                  :: s
      integer                                  :: m
      if (size(x) <= 2) then
         s = self%other%sum(x)
      else
         m = size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function pairwise_sum_i

   recursive function pairwise_sum_r(self, x) result(s)
      class(PairwiseSum_SimpleSum), intent(in) :: self
      real(real64),                 intent(in) :: x(:)
      real(real64)                             :: s
      integer                                  :: m
      if (size(x) <= 2) then
         s = self%other%sum(x)
      else
         m = size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function pairwise_sum_r

   function simple_averager_init(drv) result(res)
      type(SimpleSum), intent(in) :: drv
      type(Averager_SimpleSum)    :: res
      res%drv = drv
   end function simple_averager_init

   function simple_average_i(self, x) result(a)
      class(Averager_SimpleSum), intent(in) :: self
      integer,                   intent(in) :: x(:)
      integer                               :: a
      a = self%drv%sum(x) / size(x)
   end function simple_average_i

   function simple_average_r(self, x) result(a)
      class(Averager_SimpleSum), intent(in) :: self
      real(real64),              intent(in) :: x(:)
      real(real64)                          :: a
      a = self%drv%sum(x) / real(size(x), real64)
   end function simple_average_r

   function pairwise_averager_init(drv) result(res)
      type(PairwiseSum_SimpleSum), intent(in) :: drv
      type(Averager_PairwiseSum_SimpleSum)    :: res
      res%drv = drv
   end function pairwise_averager_init

   function pairwise_average_i(self, x) result(a)
      class(Averager_PairwiseSum_SimpleSum), intent(in) :: self
      integer,                               intent(in) :: x(:)
      integer                                           :: a
      a = self%drv%sum(x) / size(x)
   end function pairwise_average_i

   function pairwise_average_r(self, x) result(a)
      class(Averager_PairwiseSum_SimpleSum), intent(in) :: self
      real(real64),                          intent(in) :: x(:)
      real(real64)                                      :: a
      a = self%drv%sum(x) / real(size(x), real64)
   end function pairwise_average_r

end module static_instances

program main

   ! dependencies on abstractions
   use interfaces,       only: IAverager

   ! dependencies on implementations
   use simple_library,   only: SimpleSum
   use static_instances, only: PairwiseSum_SimpleSum, Averager_SimpleSum, &
                               Averager_PairwiseSum_SimpleSum

   implicit none

   ! declarations
   class(IAverager), allocatable :: av
   integer :: key
   integer, allocatable :: xi(:)
   double precision, allocatable :: xf(:)

   write(*,'(a)') 'Simple   sum average: 1'
   write(*,'(a)') 'Pairwise sum average: 2'
   write(*,'(a)',advance='no') 'Choose an averaging method: '
   read(*,*) key

   select case (key)
   case (1)
      av = Averager_SimpleSum(drv = SimpleSum())
   case (2)
      av = Averager_PairwiseSum_SimpleSum(drv = PairwiseSum_SimpleSum(other = SimpleSum()))
   case default
      stop 'Case not implemented!'
   end select

   xi = [1, 2, 3, 4, 5]
   xf = [1.d0, 2.d0, 3.d0, 4.d0, 5.d0]

   print '(i1)',   av%average(xi)
   print '(f3.1)', av%average(xf)

end program main
