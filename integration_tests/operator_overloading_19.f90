module operator_overloading_19_mod1
  implicit none

  ! Abstract base type
  type, abstract :: temp
  contains
    procedure(is_equal), deferred :: is_same
    generic :: operator(==) => is_same
  end type temp

  abstract interface
    logical function is_equal(this, other)
      import :: temp
      class(temp), intent(in) :: this, other
    end function is_equal
  end interface

end module operator_overloading_19_mod1


module operator_overloading_19_mod2
  use operator_overloading_19_mod1
  implicit none

  type, extends(temp) :: temp1
    integer :: id
  contains
    procedure :: is_same => temp1_is_same
  end type temp1

  type, extends(temp1) :: temp2
    real :: extra
  end type temp2

contains

  ! Compare two temp1 objects
  logical function temp1_is_same(this, other)
    class(temp1), intent(in) :: this
    class(temp), intent(in) :: other
    class(temp1), pointer :: other_t1

    temp1_is_same = .false.
    select type(other_t1 => other)
    type is (temp1)
      temp1_is_same = (this%id == other_t1%id)
    type is (temp2)
      temp1_is_same = (this%id == other_t1%id)
    end select
  end function temp1_is_same

end module operator_overloading_19_mod2


program operator_overloading_19
  use operator_overloading_19_mod1
  use operator_overloading_19_mod2
  ! implicit none
  class(temp2), allocatable :: a, b
  allocate(a, b)
  a%id = 5
  b%id = 5
  if (.not. a == b) error stop
end program operator_overloading_19
