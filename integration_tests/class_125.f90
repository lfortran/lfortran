! Test: class(*) allocatable array member in a type extending an abstract
! base with a deferred procedure. Accessing a%v and b%v inside select type
! must compile and run correctly.
module class_125_m
  implicit none
  type, abstract :: base_type
  contains
    procedure(sub), deferred :: copy_impl
  end type
  abstract interface
    subroutine sub(a, b)
      import base_type
      class(base_type), intent(inout) :: a
      class(base_type), intent(in) :: b
    end subroutine
  end interface
  type, extends(base_type) :: vec
    class(*), allocatable :: v(:)
  contains
    procedure :: copy_impl => vec_copy
  end type
contains
  subroutine vec_copy(a, b)
    class(vec), intent(inout) :: a
    class(base_type), intent(in) :: b
    select type (b)
    type is (vec)
      if (allocated(b%v)) then
        if (.not. allocated(a%v)) then
          allocate(integer :: a%v(size(b%v)))
        end if
      end if
    end select
  end subroutine
end module

program class_125
  use class_125_m
  implicit none
  type(vec) :: x, y
  allocate(integer :: x%v(3))
  select type (v => x%v)
  type is (integer)
    v(1) = 10
    v(2) = 20
    v(3) = 30
  end select
  call y%copy_impl(x)
  if (.not. allocated(y%v)) error stop
  if (size(y%v) /= 3) error stop
  print *, "PASS"
end program
