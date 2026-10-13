! Standard-Fortran oracle for the paper's Code/Fortran/vector.f90 (companion
! revision 5163a026, sha256
! 88d11188ce85ed6c5c4067344eb2a432f39ec742730584310a2576143d671b40; the
! acceptance copy is unchanged). test_vector creates three instances of the
! generic type Vector{U}; the oracle writes them out: Vector{real(8)} inferred
! from 0.d0, Vector{logical} named explicitly, and Vector{character(len=5)}
! inferred from "John ". In every instance the associated type Element of
! IAppendable is U. Each instance keeps the allocatable elements(:)
! component, an initializer storing [item], the mutating append(self, item)
! with an INTENT(INOUT) receiver that reallocates through
! [self%elements, item], and the list-directed printout. The generic
! interface Vector selects the instance from its argument, as the paper
! infers U from the initializer argument; the explicit Vector{logical} call
! names its instance constructor. := declarations become explicit
! declarations.
module vector_oracle_library

   implicit none
   private

   public :: Vector, Vector_double, Vector_logical, Vector_character5

   type :: Vector_double
      private
      double precision, allocatable :: elements(:)
   contains
      procedure, pass :: append => append_double
      procedure, pass :: printout => printout_double
   end type Vector_double

   type :: Vector_logical
      private
      logical, allocatable :: elements(:)
   contains
      procedure, pass :: append => append_logical
      procedure, pass :: printout => printout_logical
   end type Vector_logical

   type :: Vector_character5
      private
      character(len=5), allocatable :: elements(:)
   contains
      procedure, pass :: append => append_character5
      procedure, pass :: printout => printout_character5
   end type Vector_character5

   ! initial :: init, with U inferred from the initializer argument
   interface Vector
      module procedure init_double, init_logical, init_character5
   end interface Vector

   ! Vector{logical}(...)
   interface Vector_logical
      module procedure init_logical
   end interface Vector_logical

contains

   function init_double(item) result(res)
      double precision, intent(in) :: item
      type(Vector_double)          :: res
      res%elements = [item]
   end function init_double

   subroutine append_double(self,item)
      class(Vector_double), intent(inout) :: self
      double precision,     intent(in)    :: item
      self%elements = [self%elements,item]
   end subroutine append_double

   subroutine printout_double(self)
      class(Vector_double), intent(in) :: self
      print *, self%elements
   end subroutine printout_double

   function init_logical(item) result(res)
      logical, intent(in)  :: item
      type(Vector_logical) :: res
      res%elements = [item]
   end function init_logical

   subroutine append_logical(self,item)
      class(Vector_logical), intent(inout) :: self
      logical,               intent(in)    :: item
      self%elements = [self%elements,item]
   end subroutine append_logical

   subroutine printout_logical(self)
      class(Vector_logical), intent(in) :: self
      print *, self%elements
   end subroutine printout_logical

   function init_character5(item) result(res)
      character(len=5), intent(in) :: item
      type(Vector_character5)      :: res
      res%elements = [item]
   end function init_character5

   subroutine append_character5(self,item)
      class(Vector_character5), intent(inout) :: self
      character(len=5),         intent(in)    :: item
      self%elements = [self%elements,item]
   end subroutine append_character5

   subroutine printout_character5(self)
      class(Vector_character5), intent(in) :: self
      print *, self%elements
   end subroutine printout_character5

end module vector_oracle_library

program test_vector

   use vector_oracle_library, only: Vector, Vector_double, Vector_logical, &
                                    Vector_character5

   implicit none

   type(Vector_double)     :: doubles
   type(Vector_logical)    :: bools
   type(Vector_character5) :: strings

   doubles = Vector(0.d0)
   call doubles%append(1.5d0)
   call doubles%append(2.2d0)
   call doubles%printout()         ! prints  "0. 1.5 2.2"

   bools = Vector_logical(.true.)
   call bools%append(.false.)
   call bools%append(.true.)
   call bools%printout()           ! prints  "T F T"

   strings = Vector("John ")
   call strings%append("Mary ")
   call strings%append("Anne ")
   call strings%printout()         ! prints  "John Mary Anne "

end program test_vector
