module select_type_36_mod
  implicit none
  type :: t
    integer, allocatable :: val(:)
  contains
    procedure :: get_ptr
  end type
contains
  function get_ptr(this) result(p)
    class(t), target, intent(in) :: this
    integer, pointer :: p(:)
    p => this%val
  end function

  subroutine use_array(a, expected_sum)
    integer, intent(in) :: a(:)
    integer, intent(in) :: expected_sum
    if (sum(a) /= expected_sum) error stop
  end subroutine

  subroutine trigger(x)
    class(t), intent(in) :: x
    select type (x)
    type is (t)
      call use_array(x%get_ptr(), 10)
    end select
  end subroutine
end module

program select_type_36
  use select_type_36_mod
  implicit none
  type(t) :: obj
  allocate(obj%val(4))
  obj%val = [1, 2, 3, 4]
  call trigger(obj)
  print *, "ok"
end program
