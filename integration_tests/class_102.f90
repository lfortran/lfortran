! Test for https://github.com/lfortran/lfortran/issues/8303
! Extended type with polymorphic dispatch through self%obj%method(wrap(1)%arr)
! LLVM IR verification failed with type mismatch for allocatable array argument
module class_102_mod

   type, abstract :: AbsType
   contains
      procedure(method), deferred :: method
   end type AbsType

   abstract interface
      subroutine method(self,arr)
         import
         class(AbsType),       intent(inout) :: self
         real(8), allocatable, intent(inout) :: arr(:)
      end subroutine method
   end interface

   type, extends(AbsType) :: MyType
      class(AbsType), allocatable :: obj
   contains
      procedure :: method => implementation
      procedure :: do_work
   end type MyType

   type :: Wrapper
      real(8), allocatable :: arr(:)
   end type Wrapper

contains

   subroutine implementation(self,arr)
      class(MyType),        intent(inout) :: self
      real(8), allocatable, intent(inout) :: arr(:)
      integer :: i
      do i = 1, size(arr)
         arr(i) = arr(i) * 2.0d0
      end do
   end subroutine implementation

   subroutine do_work(self,wrap)
      class(MyType), intent(inout) :: self
      type(Wrapper), intent(inout) :: wrap(:)
      call self%obj%method(wrap(1)%arr)
   end subroutine do_work

end module class_102_mod

program class_102
   use class_102_mod
   implicit none

   type(MyType) :: obj
   type(Wrapper) :: wrap(1)

   allocate(MyType :: obj%obj)
   allocate(wrap(1)%arr(3))
   wrap(1)%arr = [1.0d0, 2.0d0, 3.0d0]

   call obj%do_work(wrap)

   if (abs(wrap(1)%arr(1) - 2.0d0) > 1.0d-10) error stop
   if (abs(wrap(1)%arr(2) - 4.0d0) > 1.0d-10) error stop
   if (abs(wrap(1)%arr(3) - 6.0d0) > 1.0d-10) error stop
   print *, "PASS"
end program class_102
