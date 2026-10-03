module procedure_41_mod1
   implicit none

   type, abstract :: AbsType
      procedure(intfc), pointer, nopass :: ptr => null()
   end type AbsType

   abstract interface
      subroutine intfc(self,n,b)
         import :: AbsType
         class(AbsType), intent(in)    :: self
         integer(4),     intent(in)    :: n
         real(8),        intent(inout) :: b(n:,n:,:)
      end subroutine intfc
   end interface

end module procedure_41_mod1

module procedure_41_mod2
   use procedure_41_mod1
   implicit none

   type, extends(AbsType) :: ConcreteType
      integer :: id
   end type ConcreteType

   type :: MyType
      class(AbsType), allocatable :: obj
   end type MyType

contains

   subroutine myproc(self,n,b)
      class(AbsType), intent(in)    :: self
      integer(4),     intent(in)    :: n
      real(8),        intent(inout) :: b(n:,n:,:)

      integer :: i,j,k

      do i = lbound(b,1), ubound(b,1)
         do j = lbound(b,2), ubound(b,2)
            do k = lbound(b,3), ubound(b,3)
               b(i,j,k) = 42.0d0
            end do
         end do
      end do
   end subroutine myproc

   subroutine client(n,b)
      integer(4), intent(in)    :: n
      real(8),    intent(inout) :: b(n:,n:,:)

      class(MyType), allocatable :: arr(:,:)

      allocate(arr(1,1))
      allocate(ConcreteType :: arr(1,1)%obj)
      associate(ob => arr(1,1)%obj)
          ob%ptr => myproc

          if (.not. associated(ob%ptr)) then
            error stop "Procedure pointer not associated"
          end if

          call ob%ptr(ob, n, b)
      end associate

      if (b(n,n,1) /= 42.0d0) then
         error stop "Procedure pointer call failed"
      end if

   end subroutine client

end module procedure_41_mod2



program procedure_41
   use procedure_41_mod2
   implicit none

   integer(4) :: n
   real(8)    :: b(1:10,1:10,1:10)

   n = 1
   b = 0.0d0

   call client(n,b)

   if (b(1,1,1) /= 42.0d0) then
      error stop "Final verification failed"
   end if

   print *, "All abstract type + procedure pointer tests passed."

end program procedure_41