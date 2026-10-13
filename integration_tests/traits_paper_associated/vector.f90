module traits

   implicit none

   abstract interface :: IAppendable
      type :: Element
      subroutine append(item)
         type(Element), intent(in) :: item
      end subroutine append
   end interface IAppendable

end module traits

module vector_library

   use traits, only: IAppendable
   
   implicit none
   private

   type, public, sealed, implements(IAppendable) :: Vector{U}
      private
      type(U), allocatable :: elements(:)
   contains
      initial :: init
      procedure, pass :: append
      procedure, pass :: printout
   end type Vector

contains

   function init(item) result(res)
      type(U), intent(in) :: item
      type(Vector{U})     :: res
      res%elements = [item]
   end function init
   
   subroutine append(self,item)
      type(Vector{U}), intent(inout) :: self
      type(U),         intent(in)    :: item
      self%elements = [self%elements,item]
   end subroutine append

   subroutine printout(self)
      type(Vector{U}), intent(in) :: self
      print *, self%elements
   end subroutine printout
   
end module vector_library

program test_vector

   use vector_library, only: Vector
   
   implicit none

   doubles := Vector(0.d0)
   call doubles%append(1.5d0)
   call doubles%append(2.2d0)
   call doubles%printout()         ! prints  "0. 1.5 2.2"

   bools := Vector{logical}(.true.)
   call bools%append(.false.)
   call bools%append(.true.)
   call bools%printout()           ! prints  "T F T"

   strings := Vector("John ")
   call strings%append("Mary ")
   call strings%append("Anne ")
   call strings%printout()         ! prints  "John Mary Anne "

end program test_vector
