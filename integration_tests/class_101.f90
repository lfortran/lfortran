! Test for https://github.com/lfortran/lfortran/issues/7640
! Deferred function on polymorphic member returned wrong results (all zeros)
! MRE from issue comment by kmr-srbh
module class_101_mod

   type, abstract :: AbsType
   contains
      procedure(method2), deferred :: method2
   end type AbsType

   abstract interface
      function method2(self,arr) result(a)
         import
         class(AbsType), intent(in) :: self
         integer,        intent(in) :: arr(:)
         integer                    :: a(size(arr))
      end function method2
   end interface

   type :: SomeType
      integer,        allocatable :: arr(:)
      class(AbsType), allocatable :: obj
   contains
      procedure :: method1
   end type SomeType

   type, extends(AbsType) :: MyType
   contains
      procedure :: method2 => my_method2
   end type MyType

contains

   subroutine method1(self)
      class(SomeType), intent(inout) :: self
      self%arr = self%obj%method2(self%arr)
   end subroutine method1

   function my_method2(self, arr) result(a)
      class(MyType), intent(in) :: self
      integer, intent(in) :: arr(:)
      integer :: a(size(arr))
      integer :: i
      do i = 1, size(arr)
         a(i) = arr(i) * 2
      end do
   end function my_method2

end module class_101_mod

program class_101
   use class_101_mod
   implicit none

   class(SomeType), allocatable :: s

   allocate(s)
   allocate(s%arr(3))
   allocate(MyType :: s%obj)
   s%arr = [1, 2, 3]

   call s%method1()

   if (s%arr(1) /= 2) error stop
   if (s%arr(2) /= 4) error stop
   if (s%arr(3) /= 6) error stop
   print *, "PASS"
end program class_101
