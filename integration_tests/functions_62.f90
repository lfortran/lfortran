program main
  print *, value()
contains
  integer function value()
    allocatable :: value
    allocate(value)
    value = 42
  end function value
end program main
