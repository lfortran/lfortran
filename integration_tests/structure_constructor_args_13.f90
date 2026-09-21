! An allocatable component with no corresponding component-data-source in a
! structure constructor is unallocated in the value the constructor builds,
! and a pointer component with none takes its default initialization
! (F2018 7.5.10). The target of an intrinsic assignment takes that value, so
! such a component of the target must not stay allocated or associated.
module structure_constructor_args_13_mod
   implicit none

   integer, target :: shared_target = 42

   type :: comp_t
      integer, allocatable :: al(:)
      integer, pointer :: p => null()
      integer :: x = 1
      integer :: y = 7
   end type comp_t

   type :: outer_t
      type(comp_t) :: c
      integer :: z = 5
   end type outer_t

   type :: base_t
      integer, allocatable :: b(:)
      integer :: bx = 3
   end type base_t

   type, extends(base_t) :: ext_t
      integer, allocatable :: e(:)
      integer :: ex = 4
   end type ext_t

   type(comp_t), parameter :: param_comp = comp_t(x=10)
   type(outer_t), parameter :: param_outer = outer_t(param_comp, 30)

contains

   ! Give every component of `v` a state that a constructor omitting them
   ! must not leave behind.
   subroutine dirty(v)
      type(comp_t), intent(inout) :: v
      if (allocated(v%al)) deallocate(v%al)
      allocate(v%al(3))
      v%al = 5
      v%p => shared_target
      v%x = 99
      v%y = 99
   end subroutine dirty

   subroutine check(v, x, code)
      type(comp_t), intent(in) :: v
      integer, intent(in) :: x, code
      if (allocated(v%al)) error stop code
      if (associated(v%p)) error stop code + 1
      if (v%x /= x) error stop code + 2
      ! `y` was not given either, so it takes its default initializer,
      ! not merely an undefined or zeroed value.
      if (v%y /= 7) error stop code + 3
   end subroutine check

end module structure_constructor_args_13_mod

program structure_constructor_args_13
   use structure_constructor_args_13_mod
   implicit none

   type(comp_t) :: v, arr(2)
   type(outer_t) :: o
   type(ext_t) :: ex
   integer :: i

   ! A literal structure constructor.
   call dirty(v)
   v = comp_t(x=20)
   call check(v, 20, 1)

   ! A named constant of the type behaves the same way.
   call dirty(v)
   v = param_comp
   call check(v, 10, 11)

   ! A component that is given a value is still assigned.
   call dirty(v)
   v = comp_t(al=[1, 2], x=30)
   if (.not. allocated(v%al)) error stop 21
   if (size(v%al) /= 2) error stop 22
   if (v%al(1) /= 1 .or. v%al(2) /= 2) error stop 23
   if (associated(v%p)) error stop 24
   if (v%x /= 30 .or. v%y /= 7) error stop 25

   ! A pointer component that is given a target stays associated.
   call dirty(v)
   v = comp_t(p=shared_target, x=31)
   if (allocated(v%al)) error stop 26
   if (.not. associated(v%p)) error stop 27
   if (v%p /= 42) error stop 28

   ! A component of a component, reached through a nested constructor.
   call dirty(o%c)
   o%z = 99
   o = outer_t(comp_t(x=40), 41)
   call check(o%c, 40, 31)
   if (o%z /= 41) error stop 35

   ! The same, from a named constant of the outer type.
   call dirty(o%c)
   o%z = 99
   o = param_outer
   call check(o%c, 10, 41)
   if (o%z /= 30) error stop 45

   ! A component inherited from a parent type.
   allocate(ex%b(2))
   allocate(ex%e(2))
   ex%bx = 99
   ex%ex = 99
   ex = ext_t(ex=9)
   if (allocated(ex%b)) error stop 51
   if (allocated(ex%e)) error stop 52
   if (ex%bx /= 3) error stop 53
   if (ex%ex /= 9) error stop 54

   ! A scalar constructor broadcast over an array target.
   call dirty(arr(1))
   call dirty(arr(2))
   arr = comp_t(x=60)
   call check(arr(1), 60, 61)
   call check(arr(2), 60, 71)

   ! The memory the target held is released, so the component can be
   ! allocated again on every pass.
   do i = 1, 100
      allocate(v%al(1000))
      v%al = i
      v = comp_t(x=i)
      if (allocated(v%al)) error stop 81
      if (v%x /= i) error stop 82
   end do

   print *, "ok"
end program structure_constructor_args_13
