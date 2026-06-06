program main
    implicit none
    integer, allocatable :: val

    val = value()
    if (val /= 42) error stop

contains

  integer function value()
    allocatable :: value

    allocate(value)
    value = 42
  end function value

end program main
