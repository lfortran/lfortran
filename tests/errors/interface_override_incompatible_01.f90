module abstypemod_ioi01

   type, abstract :: AbsType
   contains
      procedure(method), deferred :: method
   end type AbsType

   abstract interface
      subroutine method(self,a)
         import; implicit none
         class(AbsType),       intent(inout) :: self
         real(8), allocatable, intent(inout) :: a(:,:)
      end subroutine method
   end interface

end module abstypemod_ioi01

module mytypemod_ioi01

   use abstypemod_ioi01, only: AbsType

   type, extends(AbsType) :: MyType
   contains
      procedure :: method
   end type MyType

contains

   subroutine method(self,i1,a)
      class(MyType),  intent(inout) :: self
      integer, allocatable, intent(inout) :: i1(:)
      real(8), allocatable, intent(inout) :: a(:,:)
   end subroutine method

end module mytypemod_ioi01
