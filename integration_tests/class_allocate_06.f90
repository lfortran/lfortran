! Test: intrinsic assignment of class(*) array to class(*) allocatable
! component when the actual argument is also class(*).
module class_allocate_06_mod
  implicit none
  type :: box
    class(*), allocatable :: val(:)
  end type
contains
  subroutine assign_val(b, v)
    type(box), intent(out) :: b
    class(*), intent(in) :: v(:)
    b%val = v
  end subroutine
end module

program class_allocate_06
  use class_allocate_06_mod
  implicit none
  type(box) :: b
  class(*), allocatable :: arr(:)

  allocate(integer :: arr(3))
  select type(arr)
  type is (integer)
    arr = [10, 20, 30]
  end select

  call assign_val(b, arr)

  select type (x => b%val)
  type is (integer)
    if (size(x) /= 3) error stop
    if (x(1) /= 10) error stop
    if (x(2) /= 20) error stop
    if (x(3) /= 30) error stop
  class default
    error stop
  end select

  print *, "PASS"
end program
