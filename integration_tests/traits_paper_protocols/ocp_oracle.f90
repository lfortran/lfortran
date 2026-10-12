! Standard-Fortran oracle for the paper's Code/Fortran/ocp.f90 (companion
! revision 5163a026, sha256
! 7852f86195a9d44721baf930929f8704b49750d10b8dd255744d4d1e1dba7c4c; the
! acceptance copy adds one "use intrinsics" to the program). It keeps the
! unit order and roles, initializer constructor chaining, trait-valued
! allocatable components, the READ/SELECT CASE driver and the output formats.
! Unlike the closed mixed.f90 oracle, INumeric stays open: it is an abstract
! type whose deferred bindings are the trait's initializer (a NOPASS function
! of an integer) and its operators + and / on the implementing type itself.
! The libraries never name a concrete numeric type, so a later module can add
! a numeric type without recompiling them. Standard Fortran cannot extend
! intrinsic types, so ocp_oracle_intrinsics acknowledges integer and
! real(real64) through wrapper types that forward to the intrinsic
! operations, and SELECT TYPE enforces type(itself) operands. A generic
! message over T becomes one binding over class(INumeric) arrays and results
! whose dynamic type is T, and T(n) is the initializer of the dynamic type of
! the argument array. := declarations become explicit declarations.
module ocp_oracle_interfaces

   implicit none
   private

   public :: INumeric, ISum, IAverager, initial_of

   type, abstract :: INumeric
   contains
      procedure(initial_iface), deferred, nopass :: initial
      procedure(binary_iface),  deferred         :: add
      procedure(binary_iface),  deferred         :: divide
      generic :: operator(+) => add
      generic :: operator(/) => divide
   end type INumeric

   abstract interface
      function initial_iface(n) result(res)
         import :: INumeric
         integer,         intent(in)  :: n
         class(INumeric), allocatable :: res
      end function initial_iface
      function binary_iface(lhs, rhs) result(res)
         import :: INumeric
         class(INumeric), intent(in)  :: lhs, rhs
         class(INumeric), allocatable :: res
      end function binary_iface
   end interface

   type, abstract :: ISum
   contains
      procedure(sum_iface), deferred :: sum
   end type ISum

   type, abstract :: IAverager
   contains
      procedure(average_iface), deferred :: average
   end type IAverager

   abstract interface
      function sum_iface(self, x) result(s)
         import :: ISum, INumeric
         class(ISum),     intent(in)  :: self
         class(INumeric), intent(in)  :: x(:)
         class(INumeric), allocatable :: s
      end function sum_iface
      function average_iface(self, x) result(a)
         import :: IAverager, INumeric
         class(IAverager), intent(in)  :: self
         class(INumeric),  intent(in)  :: x(:)
         class(INumeric),  allocatable :: a
      end function average_iface
   end interface

contains

   ! T(n) for the element type T of x, also when x is empty.
   function initial_of(x, n) result(res)
      class(INumeric), intent(in)  :: x(:)
      integer,         intent(in)  :: n
      class(INumeric), allocatable :: res
      class(INumeric), allocatable :: probe(:)
      allocate(probe(1), mold=x)
      res = probe(1)%initial(n)
   end function initial_of

end module ocp_oracle_interfaces

module ocp_oracle_intrinsics

   use, intrinsic :: iso_fortran_env, only: real64
   use ocp_oracle_interfaces, only: INumeric

   implicit none
   private

   public :: IntegerNumeric, Real64Numeric, integer_of, real64_of

   ! implements INumeric :: integer
   type, extends(INumeric) :: IntegerNumeric
      integer :: value
   contains
      procedure, nopass :: initial => integer_initial
      procedure         :: add     => integer_add
      procedure         :: divide  => integer_divide
   end type IntegerNumeric

   ! implements INumeric :: real(real64)
   type, extends(INumeric) :: Real64Numeric
      real(real64) :: value
   contains
      procedure, nopass :: initial => real64_initial
      procedure         :: add     => real64_add
      procedure         :: divide  => real64_divide
   end type Real64Numeric

contains

   function integer_initial(n) result(res)
      integer,         intent(in)  :: n
      class(INumeric), allocatable :: res
      res = IntegerNumeric(n)
   end function integer_initial

   function integer_add(lhs, rhs) result(res)
      class(IntegerNumeric), intent(in)  :: lhs
      class(INumeric),       intent(in)  :: rhs
      class(INumeric),       allocatable :: res
      select type (rhs)
      type is (IntegerNumeric)
         res = IntegerNumeric(lhs%value + rhs%value)
      class default
         error stop 'ocp oracle: operands of + differ in type'
      end select
   end function integer_add

   function integer_divide(lhs, rhs) result(res)
      class(IntegerNumeric), intent(in)  :: lhs
      class(INumeric),       intent(in)  :: rhs
      class(INumeric),       allocatable :: res
      select type (rhs)
      type is (IntegerNumeric)
         res = IntegerNumeric(lhs%value / rhs%value)
      class default
         error stop 'ocp oracle: operands of / differ in type'
      end select
   end function integer_divide

   function real64_initial(n) result(res)
      integer,         intent(in)  :: n
      class(INumeric), allocatable :: res
      res = Real64Numeric(real(n, real64))
   end function real64_initial

   function real64_add(lhs, rhs) result(res)
      class(Real64Numeric), intent(in)  :: lhs
      class(INumeric),      intent(in)  :: rhs
      class(INumeric),      allocatable :: res
      select type (rhs)
      type is (Real64Numeric)
         res = Real64Numeric(lhs%value + rhs%value)
      class default
         error stop 'ocp oracle: operands of + differ in type'
      end select
   end function real64_add

   function real64_divide(lhs, rhs) result(res)
      class(Real64Numeric), intent(in)  :: lhs
      class(INumeric),      intent(in)  :: rhs
      class(INumeric),      allocatable :: res
      select type (rhs)
      type is (Real64Numeric)
         res = Real64Numeric(lhs%value / rhs%value)
      class default
         error stop 'ocp oracle: operands of / differ in type'
      end select
   end function real64_divide

   function integer_of(x) result(value)
      class(INumeric), intent(in) :: x
      integer                     :: value
      select type (x)
      type is (IntegerNumeric)
         value = x%value
      class default
         error stop 'ocp oracle: integer result expected'
      end select
   end function integer_of

   function real64_of(x) result(value)
      class(INumeric), intent(in) :: x
      real(real64)                :: value
      select type (x)
      type is (Real64Numeric)
         value = x%value
      class default
         error stop 'ocp oracle: real(real64) result expected'
      end select
   end function real64_of

end module ocp_oracle_intrinsics

module ocp_oracle_simple_library

   use ocp_oracle_interfaces, only: ISum, INumeric, initial_of

   implicit none
   private

   public :: SimpleSum

   type, extends(ISum) :: SimpleSum
   contains
      procedure :: sum => simple_sum
   end type SimpleSum

contains

   function simple_sum(self, x) result(s)
      class(SimpleSum), intent(in)  :: self
      class(INumeric),  intent(in)  :: x(:)
      class(INumeric),  allocatable :: s
      integer                       :: i
      s = initial_of(x, 0)
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function simple_sum

end module ocp_oracle_simple_library

module ocp_oracle_pairwise_library

   use ocp_oracle_interfaces, only: ISum, INumeric

   implicit none
   private

   public :: PairwiseSum

   type, extends(ISum) :: PairwiseSum
      private
      class(ISum), allocatable :: other
   contains
      procedure :: sum => pairwise_sum
   end type PairwiseSum

   interface PairwiseSum
      module procedure init
   end interface PairwiseSum

contains

   function init(other) result(res)
      class(ISum), intent(in) :: other
      type(PairwiseSum)       :: res
      res%other = other
   end function init

   recursive function pairwise_sum(self, x) result(s)
      class(PairwiseSum), intent(in)  :: self
      class(INumeric),    intent(in)  :: x(:)
      class(INumeric),    allocatable :: s
      integer                         :: m
      if (size(x) <= 2) then
         s = self%other%sum(x)
      else
         m = size(x) / 2
         s = self%sum(x(:m)) + self%sum(x(m+1:))
      end if
   end function pairwise_sum

end module ocp_oracle_pairwise_library

module ocp_oracle_averager_library

   use ocp_oracle_interfaces, only: IAverager, ISum, INumeric, initial_of

   implicit none
   private

   public :: Averager

   type, extends(IAverager) :: Averager
      private
      class(ISum), allocatable :: drv
   contains
      procedure :: average => averager_average
   end type Averager

   interface Averager
      module procedure init
   end interface Averager

contains

   function init(drv) result(res)
      class(ISum), intent(in) :: drv
      type(Averager)          :: res
      res%drv = drv
   end function init

   function averager_average(self, x) result(a)
      class(Averager), intent(in)  :: self
      class(INumeric), intent(in)  :: x(:)
      class(INumeric), allocatable :: a
      ! GFortran 16 rejects a defined operator applied directly to
      ! self%drv%sum(x), so the sum is named first.
      class(INumeric), allocatable :: total
      total = self%drv%sum(x)
      a = total / initial_of(x, size(x))
   end function averager_average

end module ocp_oracle_averager_library

program main

   ! dependencies on abstractions
   use ocp_oracle_interfaces,       only: IAverager

   ! dependencies on implementations
   use ocp_oracle_simple_library,   only: SimpleSum
   use ocp_oracle_pairwise_library, only: PairwiseSum
   use ocp_oracle_averager_library, only: Averager
   use ocp_oracle_intrinsics,       only: IntegerNumeric, Real64Numeric, &
                                          integer_of, real64_of

   implicit none

   ! declarations
   class(IAverager), allocatable :: av
   integer :: key
   type(IntegerNumeric) :: xi(5)
   type(Real64Numeric)  :: xf(5)

   write(*,'(a)') 'Simple   sum average: 1'
   write(*,'(a)') 'Pairwise sum average: 2'
   write(*,'(a)',advance='no') 'Choose an averaging method: '
   read(*,*) key

   select case (key)
   case (1)
      av = Averager(drv = SimpleSum())
   case (2)
      av = Averager(drv = PairwiseSum(other = SimpleSum()))
   case default
      stop 'Case not implemented!'
   end select

   xi = [IntegerNumeric(1), IntegerNumeric(2), IntegerNumeric(3), &
         IntegerNumeric(4), IntegerNumeric(5)]
   xf = [Real64Numeric(1.d0), Real64Numeric(2.d0), Real64Numeric(3.d0), &
         Real64Numeric(4.d0), Real64Numeric(5.d0)]

   print '(i1)',   integer_of(av%average(xi))
   print '(f3.1)', real64_of(av%average(xf))

end program main
