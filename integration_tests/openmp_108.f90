program openmp_108
  ! A pointer bounds remapping onto a section of columns in a DO CONCURRENT
  ! loop with a LOCAL pointer (F2018 locality spec), lowered with --openmp.
  implicit none
  real, target :: b(2, 100)
  real :: v(99)
  integer :: i
  do i = 1, 100
    b(:, i) = i
  end do
  call remap_section_concurrent(b, v)
  do i = 1, 99
    if (nint(v(i)) /= i + 10 * (i + 1)) error stop 1
  end do
  print *, "ok"
contains
  subroutine remap_section_concurrent(c, r)
    real, target, intent(in) :: c(2, 100)
    real, intent(out) :: r(99)
    real, pointer :: q(:)
    integer :: i
    do concurrent (i = 1:99) local(q)
      q(1:4) => c(:, i:i+1)
      r(i) = q(2) + 10 * q(4)
    end do
  end subroutine
end program
