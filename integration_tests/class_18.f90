module class_18_mod
   use iso_fortran_env
   implicit none

   type KeywordEnforcer
   end type KeywordEnforcer

   public :: Integer32Complex32Map
   public :: Integer32Complex32Pair

   type :: Integer32Complex32Pair
      integer(kind=INT32) :: first
      complex(kind=REAL32) :: second
   end type Integer32Complex32Pair

   type, abstract :: map_s_BaseNode
   end type map_s_BaseNode

   type, extends(map_s_BaseNode) ::  map_s_Node
      type(map_s_Node), pointer :: parent => null()
   end type map_s_Node

   type :: map_Set
      private
      class(map_s_BaseNode), allocatable :: root
   contains
      procedure :: insert_single => map_s_insert_single
      generic :: insert => insert_single
   end type map_Set

   type :: Integer32Complex32Map
      private
      type(map_Set) :: tree
   contains
      procedure :: of => map_of
   end type Integer32Complex32Map
   contains

   subroutine map_s_insert_single(this, value, unused, is_new)
      class(map_Set), target, intent(inout) :: this
      type(Integer32Complex32Pair), intent(in) :: value
      integer, optional :: unused
      logical, optional, intent(out) :: is_new
      if (present(is_new)) then
         is_new = .true.
      end if

      if (present(unused)) then
         unused = 21
      end if
   end subroutine map_s_insert_single

   subroutine map_of(this, key)
      class(Integer32Complex32Map), target, intent(inout) :: this
      integer(kind=INT32), intent(in) :: key
      type(Integer32Complex32Pair) :: p
      logical :: is_new1, is_new2, is_new3
      integer :: unused1, unused2, unused3
      p%first= key
      call this%tree%insert(p, is_new=is_new1)
      if (is_new1 .eqv. .false.) error stop

      call this%tree%insert(p, unused=unused1)
      if (unused1 /= 21) error stop

      call this%tree%insert(p, is_new=is_new2, unused=unused2)
      if (is_new2 .eqv. .false.) error stop
      if (unused2 /= 21) error stop

      call this%tree%insert(p, unused=unused3, is_new=is_new3)
      if (is_new3 .eqv. .false.) error stop
      if (unused3 /= 21) error stop
   end subroutine map_of
end module class_18_mod

program class_18
    use class_18_mod
    implicit none
    type(Integer32Complex32Map) :: m
    call m%of(42)
end program
