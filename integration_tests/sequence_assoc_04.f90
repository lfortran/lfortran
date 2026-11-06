module sequence_assoc_04_mod
  implicit none
contains
  subroutine fill(n, m, a)
    integer, intent(in) :: n, m
    real, intent(inout) :: a(n, m)
    if (size(a) /= n*m) error stop "size(a)"
  end subroutine
end module

program sequence_assoc_04
  ! A fixed-size actual argument that is too small for a non-empty
  ! explicit-shape dummy must be rejected: the dummy needs 4 elements and the
  ! actual argument provides 2. The dummy extents are dummy arguments, so the
  ! frontend cannot diagnose this at compile time. This is the fixed-size
  ! counterpart of sequence_assoc_03, which covers a descriptor actual.
  use sequence_assoc_04_mod
  implicit none
  real :: c(2, 1)
  c = 0.0
  call fill(2, 2, c)
  print *, "not reached"
end program
