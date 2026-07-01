program assumed_rank_10
  implicit none

  type :: item
    integer :: x
  end type

  class(*), allocatable :: value
  type(item) :: mold

  value = item(7)
  call check(value)

contains

  subroutine check(arg)
    class(*), intent(in) :: arg(..)

    print *, same_type_as(arg, mold)
    if (.not. same_type_as(arg, mold)) error stop

    print *, extends_type_of(arg, mold)
    if (.not. extends_type_of(arg, mold)) error stop
  end subroutine check

end program 