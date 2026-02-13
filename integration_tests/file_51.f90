program file_51
  implicit none

  logical :: l_1d(8), l_2d(2,2), l_3d(2,2,2)
  logical :: larray(8)

  integer :: i10, i, j, k

  open (newunit=i10, file='file_51_direct.dat', access='direct', recl=40, status='unknown')

  ! Write record 1: all true
  larray = .true.
  write (i10, rec=1) larray

  ! Write record 2: all false
  larray = .false.
  write (i10, rec=2) larray

  ! --- Test 1: Element-by-element read from multiple arrays ---
  l_1d = .false.; l_2d = .false.; l_3d = .false.
  read (i10, rec=1) l_1d(1), l_1d(2), l_2d(1,2), l_2d(2,2), l_3d(1,1,2), &
    l_3d(2,1,2), l_1d(7), l_1d(8)

  if (.not. all(l_1d(1:2))) error stop "Test 1a: l_1d(1:2) should be true"
  if (.not. all(l_2d(1:2,2))) error stop "Test 1b: l_2d(1:2,2) should be true"
  if (.not. all(l_3d(1:2,1,2))) error stop "Test 1c: l_3d(1:2,1,2) should be true"
  if (.not. all(l_1d(7:8))) error stop "Test 1d: l_1d(7:8) should be true"

  ! Verify elements that were NOT read remain false
  if (any(l_1d(3:6))) error stop "Test 1e: l_1d(3:6) should be false"
  if (any(l_2d(1:2,1))) error stop "Test 1f: l_2d(1:2,1) should be false"

  ! --- Test 2: Nested implied do loop read ---
  l_3d = .true.
  read (i10, rec=2) (((l_3d(j,k,i), i=1,2), k=1,2), j=1,2)

  if (any(l_3d)) error stop "Test 2: l_3d should be all false after reading record 2"

  close (i10, status='delete')

  print *, "All tests passed."

end program file_51
