! Types that are not generic extend the instance Simple{Basic} of a generic
! type (TD-45): they inherit its components, bindings and conformance to ISum,
! override its generic and ordinary bindings, are dispatched to through the
! instance, and satisfy ISum as type arguments of another generic type.
module traits_generic_type_extends_04_traits
   implicit none
   abstract interface :: INum
      integer | real(8)
   end interface INum
   abstract interface :: ISum
      function sum{INum :: T}(self, x) result(s)
         type(itself), intent(in), pass :: self
         type(T), intent(in) :: x(:)
         type(T) :: s
      end function sum
   end interface ISum
end module traits_generic_type_extends_04_traits

module traits_generic_type_extends_04_basic
   use traits_generic_type_extends_04_traits, only: ISum, INum
   implicit none
   private
   public :: Basic
   type, implements(ISum) :: Basic
   contains
      procedure :: sum
   end type Basic
contains
   function sum{INum :: T}(self, x) result(s)
      class(Basic), intent(in) :: self
      type(T), intent(in) :: x(:)
      type(T) :: s
      integer :: i
      s = T(0)
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function sum
end module traits_generic_type_extends_04_basic

module traits_generic_type_extends_04_simple
   use traits_generic_type_extends_04_traits, only: ISum, INum
   implicit none
   private
   public :: Simple
   type, implements(ISum) :: Simple{ISum :: U}
      type(U), allocatable :: basic
   contains
      procedure :: sum
      procedure :: tag
   end type Simple
contains
   function sum{INum :: T}(self, x) result(s)
      class(Simple{U}), intent(in) :: self
      type(T), intent(in) :: x(:)
      type(T) :: s
      s = self%basic%sum(x)
   end function sum
   function tag(self) result(r)
      class(Simple{U}), intent(in) :: self
      integer :: r
      r = 1
   end function tag
end module traits_generic_type_extends_04_simple

module traits_generic_type_extends_04_children
   use traits_generic_type_extends_04_traits, only: ISum, INum
   use traits_generic_type_extends_04_basic, only: Basic
   use traits_generic_type_extends_04_simple, only: Simple
   implicit none
   private
   public :: Doubled, Plain, use_parent
   ! Overrides the generic binding and a plain one.
   type, extends(Simple{Basic}) :: Doubled
      integer :: factor = 2
   contains
      procedure :: sum
      procedure :: tag
   end type Doubled
   ! Inherits everything, also the conformance to ISum.
   type, extends(Simple{Basic}) :: Plain
   end type Plain
contains
   function sum{INum :: T}(self, x) result(s)
      class(Doubled), intent(in) :: self
      type(T), intent(in) :: x(:)
      type(T) :: s
      s = T(self%factor) * self%basic%sum(x)
   end function sum
   function tag(self) result(r)
      class(Doubled), intent(in) :: self
      integer :: r
      r = 2
   end function tag
   ! Dispatches through the parent instance.
   integer function use_parent(p) result(r)
      class(Simple{Basic}), intent(in) :: p
      r = p%sum([1, 2, 3]) + 100 * p%tag()
   end function use_parent
end module traits_generic_type_extends_04_children

module traits_generic_type_extends_04_client
   use traits_generic_type_extends_04_traits, only: ISum, INum
   implicit none
   private
   public :: Total
   type :: Total{ISum :: U}
      type(U), allocatable :: drv
   contains
      procedure :: run
   end type Total
contains
   function run{INum :: T}(self, x) result(s)
      class(Total{U}), intent(in) :: self
      type(T), intent(in) :: x(:)
      type(T) :: s
      s = self%drv%sum(x)
   end function run
end module traits_generic_type_extends_04_client

program traits_generic_type_extends_04
   use traits_generic_type_extends_04_basic, only: Basic
   use traits_generic_type_extends_04_children, only: Doubled, Plain, use_parent
   use traits_generic_type_extends_04_client, only: Total
   implicit none
   type(Doubled) :: d
   type(Plain) :: p
   type(Total{Doubled}) :: td
   type(Total{Plain}) :: tp
   d%basic = Basic()
   p%basic = Basic()
   if (d%sum([1, 2, 3]) /= 12) error stop
   if (abs(d%sum([1.d0, 2.d0]) - 6.d0) > 1.d-12) error stop
   if (p%sum([1, 2, 3]) /= 6) error stop
   if (d%tag() /= 2 .or. p%tag() /= 1) error stop
   if (use_parent(d) /= 212) error stop
   if (use_parent(p) /= 106) error stop
   td%drv = d
   tp%drv = p
   if (td%run([4, 5]) /= 18) error stop
   if (tp%run([4, 5]) /= 9) error stop
   if (abs(tp%run([0.5d0, 0.25d0]) - 0.75d0) > 1.d-12) error stop
   print *, "ok"
end program traits_generic_type_extends_04
