! Test: two contained subroutines with same-named local derived types
! combined with class(*) polymorphic allocation. Verifies that the
! LLVM codegen correctly distinguishes types in different scopes.

module select_type_39_mod
  implicit none
  type :: container
    class(*), allocatable :: value(:)
  end type
  interface container
    module procedure container_init
  end interface
contains
  function container_init(value) result(obj)
    class(*), intent(in) :: value(:)
    type(container) :: obj
    allocate(obj%value, source=value)
  end function
  subroutine get_value(this, value)
    type(container), intent(in) :: this
    class(*), allocatable, intent(out) :: value(:)
    allocate(value, source=this%value)
  end subroutine
end module

program select_type_39
  use select_type_39_mod
  implicit none
  call test_sub1
  call test_sub2
contains
  subroutine test_sub1
    type(container) :: x
    class(*), allocatable :: val(:)
    type :: point
      real :: x, y
    end type
    x = container([point(1.0, 2.0), point(3.0, 4.0)])
    call get_value(x, val)
    select type (val)
    type is (point)
      if (abs(val(1)%x - 1.0) > 1e-6) error stop
      if (abs(val(1)%y - 2.0) > 1e-6) error stop
      if (abs(val(2)%x - 3.0) > 1e-6) error stop
      if (abs(val(2)%y - 4.0) > 1e-6) error stop
    class default
      error stop
    end select
  end subroutine

  subroutine test_sub2
    type(container) :: x
    class(*), allocatable :: val(:)
    type :: point
      real :: x, y
    end type
    x = container([point(5.0, 6.0)])
    call get_value(x, val)
    select type (val)
    type is (point)
      if (abs(val(1)%x - 5.0) > 1e-6) error stop
      if (abs(val(1)%y - 6.0) > 1e-6) error stop
    class default
      error stop
    end select
  end subroutine
end program
