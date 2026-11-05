module sequence_assoc_02_mod
  implicit none
contains
  subroutine fill(n, m, a)
    integer, intent(in) :: n, m
    real, intent(inout) :: a(n, 2*m)
    if (size(a, 1) /= n) error stop "size(a, 1)"
    if (size(a, 2) /= 2*m) error stop "size(a, 2)"
    a = 3.0
  end subroutine

  subroutine empty_extent(n, a)
    integer, intent(in) :: n
    real, intent(inout) :: a(n, 0)
    if (size(a) /= 0) error stop "size(a)"
    if (size(a, 1) /= n) error stop "size(a, 1)"
    if (size(a, 2) /= 0) error stop "size(a, 2)"
  end subroutine

  subroutine count_elements(a)
    real, intent(in) :: a(:)
    if (size(a) /= 0) error stop "size(a)"
  end subroutine
end module

subroutine fill_ext(n, m, a)
  implicit none
  integer, intent(in) :: n, m
  real, intent(inout) :: a(n, 2*m)
  a(n, 2*m) = 7.0
end subroutine

program sequence_assoc_02
  use sequence_assoc_02_mod
  implicit none
  real, allocatable :: b(:,:)
  real :: c(4, 2)
  real, allocatable :: empty(:), empty_2d(:,:)
  integer :: i, j
  allocate(b(4, 2))
  b = 0.0
  c = 0.0
  ! A whole array actual argument associated with an explicit-shape dummy of a
  ! different shape but the same total size is valid sequence association
  ! (F2023 15.5.2.12): only the dummy's total size must not exceed the actual
  ! argument's element sequence.
  call fill(2, 2, b)
  if (abs(b(4, 2) - 3.0) > 1.0e-5) error stop "allocatable actual"
  call fill(2, 2, c)
  if (abs(c(4, 2) - 3.0) > 1.0e-5) error stop "fixed-size actual"
  b = 0.0
  call fill_ext(2, 2, b)
  if (abs(b(4, 2) - 7.0) > 1.0e-5) error stop "external procedure actual"
  ! A zero-size actual argument must not be rejected by the argument size
  ! check, for an assumed-shape dummy or for an explicit-shape dummy whose
  ! total size is also zero.
  allocate(empty(0))
  call count_elements(empty)
  allocate(empty_2d(0, 0))
  call empty_extent(2, empty_2d)
  print *, "pass"
end program
