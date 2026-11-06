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

  subroutine neg_extent(n, m, a)
    integer, intent(in) :: n, m
    real, intent(inout) :: a(n, m)
    if (size(a) /= 0) error stop "size(a)"
  end subroutine

  subroutine neg_bounds(a)
    real, intent(inout) :: a(5:1, 5:1)
    if (size(a) /= 0) error stop "size(a)"
  end subroutine
end module

module sequence_assoc_02_bindc_mod
  use iso_c_binding, only: c_float
  implicit none
contains
  subroutine bindc_assumed_size(n, a) bind(c)
    integer, intent(in) :: n
    real(c_float), intent(inout) :: a(n, *)
    if (size(a, 1) /= n) error stop "size(a, 1)"
    a(n, 1) = 5.0_c_float
    a(1, 2) = 6.0_c_float
  end subroutine
  subroutine bindc_assumed_size_count(n, a) bind(c)
    integer, intent(in) :: n
    real(c_float), intent(in) :: a(n, *)
    if (size(a, 1) /= n) error stop "size(a, 1)"
  end subroutine
  subroutine bindc_no_extent(a) bind(c)
    real(c_float), intent(inout) :: a(*)
    a(1) = 7.0_c_float
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
  use sequence_assoc_02_bindc_mod, only: bindc_assumed_size, bindc_no_extent, &
      bindc_assumed_size_count
  use iso_c_binding, only: c_float
  implicit none
  real, allocatable :: b(:,:)
  real :: c(4, 2)
  real :: z(0, 0)
  real(c_float) :: x(6)
  real(c_float) :: x30(3, 0), x40(4, 0)
  real(c_float), allocatable :: xa(:)
  real(c_float), allocatable :: xa30(:,:)
  real, allocatable :: empty(:), empty_2d(:,:)
  integer :: i, j, k, l
  allocate(b(4, 2))
  b = 0.0
  c = 0.0
  z = 0.0
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
  ! A negative runtime extent means a zero-size dimension, so the dummy's
  ! element count is zero, never a positive product of two negative extents.
  ! Both argument casts are covered: a fixed-size and a descriptor actual. The
  ! extents come from variables so the clamp is evaluated at run time instead
  ! of being constant-folded.
  k = -2
  l = k
  call neg_extent(k, l, z)
  call neg_extent(k, l, empty_2d)
  ! A negative extent in the dummy's own declaration is likewise zero-size.
  ! The descriptor actual reaches the argument check; the fixed-size actual
  ! is passed without a physical cast and only proves the declaration is
  ! accepted.
  call neg_bounds(z)
  call neg_bounds(empty_2d)
  call empty_extent(2, empty_2d)
  ! A bind(c) assumed-size dummy that still declares its first extent is
  ! checked against that extent: `a(n, *)` with n = 3 needs 3 elements.
  ! `a(*)` alone declares no extent and is not checked.
  x = 0.0_c_float
  call bindc_assumed_size(3, x)
  if (abs(x(3) - 5.0_c_float) > 1.0e-5) error stop "bind(c) a(n, 1)"
  if (abs(x(4) - 6.0_c_float) > 1.0e-5) error stop "bind(c) a(1, 2)"
  ! The same conforming call through a descriptor (allocatable) actual argument.
  allocate(xa(6))
  xa = 0.0_c_float
  call bindc_assumed_size(3, xa)
  if (abs(xa(3) - 5.0_c_float) > 1.0e-5) error stop "bind(c) allocatable a(n, 1)"
  if (abs(xa(4) - 6.0_c_float) > 1.0e-5) error stop "bind(c) allocatable a(1, 2)"
  call bindc_no_extent(x)
  if (abs(x(1) - 7.0_c_float) > 1.0e-5) error stop "bind(c) a(*)"
  ! A zero-element actual argument gives a zero-size assumed-size dummy, even
  ! though the dummy declares its first extent (F2023 15.5.2.12p6).
  call bindc_assumed_size_count(3, x30)
  call bindc_assumed_size_count(4, x40)
  allocate(xa30(3, 0))
  call bindc_assumed_size_count(3, xa30)
  print *, "pass"
end program
