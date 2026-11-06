module sequence_assoc_03_mod
  implicit none
contains
  subroutine fill(a)
    real, intent(inout) :: a(2, 2)
    a(1, 1) = 1.0
  end subroutine
end module

program sequence_assoc_03
  ! An empty actual argument must still be rejected against a non-empty
  ! explicit-shape dummy: the dummy needs 4 elements and the actual argument
  ! provides none.
  use sequence_assoc_03_mod
  implicit none
  real, allocatable :: a(:)
  allocate(a(0))
  call fill(a)
  print *, "not reached"
end program
