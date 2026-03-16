! Test: implied-do array constructor with elemental function on array args.
! The per-iteration element count must equal the size of each array argument,
! not 1.  [(f(x(:,i)), i=1,N)] where size(x(:,i))=M should produce M*N elems.
module implied_do_loops23_mod
  implicit none

  type :: result_t
    logical :: passed
    character(len=:), allocatable :: msg
  end type

contains

  elemental function check(a, b) result(res)
    real, intent(in) :: a, b
    type(result_t) :: res
    res%passed = abs(a - b) <= 0.01
    res%msg = ""
  end function

end module

program implied_do_loops23
  use implied_do_loops23_mod
  implicit none

  type(result_t), allocatable :: diags(:)
  real :: a(2, 2), b(2, 2)
  real, allocatable :: c(:)
  integer :: i

  a = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
  b = a

  ! --- elemental function returning derived type with allocatable component ---
  diags = [(check(a(:, i), b(:, i)), i = 1, 2)]
  if (size(diags) /= 4) error stop "diags: wrong size"
  do i = 1, 4
    if (.not. diags(i)%passed) error stop "diags: check failed"
  end do

  ! --- plain array binop inside implied-do ---
  c = [(a(:, i) + b(:, i), i = 1, 2)]
  if (size(c) /= 4) error stop "c: wrong size"
  if (abs(c(1) - 2.0) > 1.0e-6) error stop "c(1)"
  if (abs(c(2) - 4.0) > 1.0e-6) error stop "c(2)"
  if (abs(c(3) - 6.0) > 1.0e-6) error stop "c(3)"
  if (abs(c(4) - 8.0) > 1.0e-6) error stop "c(4)"

  print *, "All tests passed."
end program
