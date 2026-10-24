program openmp_107
  ! A pointer bounds remapping onto a whole rank-2 array, a section of
  ! columns and a CONTIGUOUS assumed-shape dummy, in parallel loops with a
  ! private pointer.
  implicit none
  real, target :: b(2, 100)
  real :: v(99)
  integer :: i
  do i = 1, 100
    b(:, i) = i
  end do
  call remap_whole(v)
  do i = 1, 99
    if (nint(v(i)) /= (i - 1) / 2 + 1 + 1000 * ((99 + i) / 2 + 1)) error stop 1
  end do
  call remap_section(b, v)
  do i = 1, 99
    if (nint(v(i)) /= i + 10 * (i + 1)) error stop 2
  end do
  call remap_dummy(b, v)
  do i = 1, 99
    if (nint(v(i)) /= i + 10 * (i + 1)) error stop 3
  end do
  print *, "ok"
contains
  subroutine remap_whole(r)
    real, intent(out) :: r(99)
    real, target :: c(2, 100)
    real, pointer :: q(:, :)
    integer :: i
    do i = 1, 100
      c(:, i) = i
    end do
    !$omp parallel do private(q)
    do i = 1, 99
      q(1:100, 1:2) => c
      r(i) = q(i, 1) + 1000 * q(i, 2)
    end do
    !$omp end parallel do
  end subroutine

  subroutine remap_section(c, r)
    real, target, intent(in) :: c(2, 100)
    real, intent(out) :: r(99)
    real, pointer :: q(:)
    integer :: i
    !$omp parallel do private(q)
    do i = 1, 99
      q(1:4) => c(:, i:i+1)
      r(i) = q(1) + 10 * q(3)
    end do
    !$omp end parallel do
  end subroutine

  subroutine remap_dummy(c, r)
    real, target, contiguous, intent(in) :: c(:, :)
    real, intent(out) :: r(99)
    real, pointer :: q(:)
    integer :: i
    !$omp parallel do private(q)
    do i = 1, 99
      q(0:199) => c
      r(i) = q(2 * i - 2) + 10 * q(2 * i + 1)
    end do
    !$omp end parallel do
  end subroutine
end program
