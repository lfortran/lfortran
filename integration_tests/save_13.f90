program save_13
  implicit none
  integer, parameter :: k(5) = [-1, 2, 3, 3, 3], ksize = size(k)
  integer :: n
  integer :: diffs1(ksize-1) = [(abs(k(n)-k(n-1)), n=2,ksize)]
  integer, parameter :: diffs_p(ksize-1) = [(abs(k(n)-k(n-1)), n=2,ksize)]

  if (diffs1(1) /= 3) error stop
  if (diffs1(2) /= 1) error stop
  if (diffs1(3) /= 0) error stop
  if (diffs1(4) /= 0) error stop

  if (diffs_p(1) /= 3) error stop
  if (diffs_p(2) /= 1) error stop
  if (diffs_p(3) /= 0) error stop
  if (diffs_p(4) /= 0) error stop

  call subprogram_save
  call subprogram_param
contains
  subroutine subprogram_save
    integer, save :: n, diffs2(ksize-1) = [(abs(k(n)-k(n-1)), n=2,ksize)]
    if (diffs2(1) /= 3) error stop
    if (diffs2(2) /= 1) error stop
    if (diffs2(3) /= 0) error stop
    if (diffs2(4) /= 0) error stop
  end subroutine subprogram_save
  subroutine subprogram_param
    integer :: i
    do i = 1, ksize - 1
      if (i == 1 .and. diffs_p(i) /= 3) error stop
      if (i == 2 .and. diffs_p(i) /= 1) error stop
      if (i == 3 .and. diffs_p(i) /= 0) error stop
      if (i == 4 .and. diffs_p(i) /= 0) error stop
    end do
  end subroutine subprogram_param
end program save_13
