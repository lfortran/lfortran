! Test: generic type-bound procedure resolution by rank
! with class(*) allocatable arguments.
module generic_name_06_mod
  implicit none
  type :: t
  contains
    generic :: get => get1d, get2d
    procedure :: get1d
    procedure :: get2d
  end type
contains
  subroutine get1d(this, x)
    class(t), intent(in) :: this
    class(*), allocatable, intent(out) :: x(:)
    allocate(integer :: x(3))
    select type(x)
    type is (integer)
      x = [10, 20, 30]
    end select
  end subroutine
  subroutine get2d(this, x)
    class(t), intent(in) :: this
    class(*), allocatable, intent(out) :: x(:,:)
    allocate(real :: x(2,2))
    select type(x)
    type is (real)
      x(1,:) = [1.0, 2.0]
      x(2,:) = [3.0, 4.0]
    end select
  end subroutine
end module

program generic_name_06
  use generic_name_06_mod
  implicit none
  type(t) :: obj
  class(*), allocatable :: vec(:)
  class(*), allocatable :: matrix(:,:)

  call obj%get(matrix)
  select type(matrix)
  type is (real)
    if (any(matrix(1,:) /= [1.0, 2.0])) error stop
    if (any(matrix(2,:) /= [3.0, 4.0])) error stop
  class default
    error stop
  end select

  call obj%get(vec)
  select type(vec)
  type is (integer)
    if (any(vec /= [10, 20, 30])) error stop
  class default
    error stop
  end select

  print *, "PASS"
end program
