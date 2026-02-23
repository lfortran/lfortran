module class_121_mod
   implicit none

   type :: MyType
   contains
      procedure :: method1
      procedure :: method2
   end type MyType

contains

   subroutine method1(self)
      class(MyType), intent(inout) :: self
      real(8), dimension(140) :: a
      real(8), dimension(140,4) :: b
      a = 1.0d0
      b = 2.0d0
      call self%method2(a(1:140), b(1:140,:))
   end subroutine method1

   subroutine method2(self, a, b)
      class(MyType), intent(in)    :: self
      real(8),       intent(in)    :: a(:)
      real(8),       intent(inout) :: b(:,:)
      b(:,1) = a + b(:,1)
   end subroutine method2

end module class_121_mod

program class_121
   use class_121_mod
   implicit none
   type(MyType) :: obj
   call obj%method1()
   print *, "ok"
end program class_121
