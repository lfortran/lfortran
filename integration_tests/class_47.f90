module class_47_mod

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

   type, extends(AbsType) :: MyType
   contains
      procedure :: method2 => my_method2
   end type MyType

   type :: SomeType
      integer,        allocatable :: arr(:)
      class(MyType), allocatable :: obj
   contains
      procedure :: method1
   end type SomeType

contains

   subroutine method1(self)
      class(SomeType), intent(inout) :: self
      allocate(MyType :: self%obj)
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

end module class_47_mod

program class_47
   use class_47_mod
   implicit none

   class(SomeType), allocatable :: s

   allocate(s)
   allocate(s%arr(3))
   s%arr = [1, 2, 3]

   call s%method1()
   print *, "s%arr: ", s%arr

   if (.not. all(s%arr == [2, 4, 6])) error stop

end program class_47