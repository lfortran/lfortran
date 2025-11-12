program pointer_04
  type :: base
  end type
  class(base), allocatable :: x
  class(base), pointer :: y
  allocate(y)
  allocate(x, source=y)
end program pointer_04
