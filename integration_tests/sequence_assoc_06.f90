module sequence_assoc_06_mod
  use iso_c_binding, only: c_float
  implicit none
contains
  subroutine bindc_assumed_size(n, a) bind(c)
    integer, intent(in) :: n
    real(c_float), intent(inout) :: a(n, *)
    a(n, 1) = 5.0_c_float
  end subroutine
end module

program sequence_assoc_06
  ! A `bind(c)` assumed-size dummy `a(n, *)` declares n as its first extent,
  ! so a descriptor (allocatable) actual argument that is too small must be
  ! rejected. This is the descriptor-actual counterpart of sequence_assoc_05,
  ! which covers a fixed-size actual; the two need separate programs because a
  ! runtime-error test stops at its first diagnostic. `--fast` turns the
  ! runtime bound checks off.
  use sequence_assoc_06_mod
  use iso_c_binding, only: c_float
  implicit none
  real(c_float), allocatable :: z(:)
  allocate(z(2))
  z = 0.0_c_float
  call bindc_assumed_size(3, z)
  print *, "not reached"
end program
