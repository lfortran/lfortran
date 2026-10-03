! Test sequence association: passing a scalar element of a 2D array
! to an assumed-size dummy. The callee indexes the original contiguous
! memory with its own leading dimension (ldc).
program legacy_array_sections_20
  implicit none
  real :: a(3, 3)
  integer :: i, j

  a = 0.0
  call set_val(a(2,2), 3, 7.0)

  ! c(1,2) in the callee maps to a(2,3) in the caller:
  ! a(2,2) is element 5 (column-major), c(1,2) = ptr + ldc = ptr + 3 = element 8 = a(2,3)
  if (a(2,3) /= 7.0) error stop

  ! Verify no other element was modified
  do j = 1, 3
    do i = 1, 3
      if (i == 2 .and. j == 3) cycle
      if (a(i,j) /= 0.0) error stop
    end do
  end do

  ! Test with a different starting element
  a = 0.0
  call set_val(a(1,1), 3, 42.0)
  if (a(1,2) /= 42.0) error stop

  print *, "ok"

contains
  subroutine set_val(c, ldc, val)
    integer, intent(in) :: ldc
    real, intent(inout) :: c(ldc, *)
    real, intent(in) :: val
    c(1, 2) = val
  end subroutine
end program
