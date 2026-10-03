! Test that same-named derived types in different modules do not collide.
! Regression test: name2dertype map keys were not module-qualified,
! causing GEP type mismatches when two modules defined types with
! the same name (one extending a parent type).

module derived_types_109_parent_m
  implicit none
  type, abstract :: parent_t
    integer :: val
  contains
    procedure(free_i), deferred :: free
  end type
  abstract interface
    impure elemental subroutine free_i(self)
      import parent_t
      class(parent_t), intent(inout) :: self
    end subroutine
  end interface
end module

module derived_types_109_mod_a
  implicit none
  type :: object_t
    integer :: dummy
  end type
contains
  subroutine check_a(x)
    type(object_t), intent(out) :: x
    x%dummy = 1
  end subroutine
end module

module derived_types_109_mod_b
  use derived_types_109_parent_m, only: parent_t
  implicit none
  type, extends(parent_t) :: object_t
  contains
    procedure :: free
  end type
contains
  impure elemental subroutine free(self)
    class(object_t), intent(inout) :: self
    self%val = 0
  end subroutine
  subroutine check_b(obj)
    type(object_t), intent(out) :: obj
    obj%val = 42
  end subroutine
end module

program derived_types_109
  use derived_types_109_mod_a, only: check_a, object_t_a => object_t
  use derived_types_109_mod_b, only: check_b, object_t_b => object_t
  implicit none
  type(object_t_a) :: a
  type(object_t_b) :: b
  call check_a(a)
  call check_b(b)
  if (a%dummy /= 1) error stop
  if (b%val /= 42) error stop
  print *, "PASS"
end program
