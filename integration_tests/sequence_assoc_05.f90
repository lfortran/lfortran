module sequence_assoc_05_mod
  use iso_c_binding, only: c_float
  implicit none
contains
  subroutine bindc_assumed_size(n, a) bind(c)
    integer, intent(in) :: n
    real(c_float), intent(inout) :: a(n, *)
    a(n, 1) = 5.0_c_float
  end subroutine
end module

program sequence_assoc_05
  ! A `bind(c)` assumed-size dummy `a(n, *)` declares n as its first extent,
  ! so a fixed-size actual argument that is too small must be rejected. The
  ! dummy extents are dummy arguments, so the frontend cannot diagnose this
  ! at compile time. `--fast` turns the runtime bound checks off.
  use sequence_assoc_05_mod
  use iso_c_binding, only: c_float
  implicit none
  real(c_float) :: y(2)
  y = 0.0_c_float
  call bindc_assumed_size(3, y)
  print *, "not reached"
end program
