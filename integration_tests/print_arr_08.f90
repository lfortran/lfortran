! Non-Unit Stride Access in Print Statements
! Testing various array section prints for 2D and 3D arrays,
! Including multi-dimensional slices and complete array prints.
! Test using write statements with string comparison.
program print_arr_08
  implicit none
  integer :: list_2d(2,3)
  integer :: list_3d(3,3,3)
  character(len=100) :: str1

  !Test 2D Array Section
  !Assign Row Values
  list_2d(1,:) = [2,2,2]
  list_2d(2,:) = [3,3,3]

  !Row-Access: concatenate arrays to print on one line
  write(str1, "(6(1X,I0))") [list_2d(1,:), list_2d(2,:)]
  print "(6(1X,I0))", [list_2d(1,:), list_2d(2,:)]
  if (trim(str1) /= " 2 2 2 3 3 3") error stop

  !Column-Access: concatenate arrays to print on one line
  write(str1, "(6(1X,I0))") [list_2d(:,1), list_2d(:,2), list_2d(:,3)]
  print "(6(1X,I0))", [list_2d(:,1), list_2d(:,2), list_2d(:,3)]
  if (trim(str1) /= " 2 3 2 3 2 3") error stop

  !Test 3D Array Section
  list_3d(1,:,1) = [4,4,4]
  list_3d(1,:,2) = [5,5,5]
  list_3d(1,:,3) = [6,6,6]
  list_3d(2,:,1) = [7,7,7]
  list_3d(2,:,2) = [8,8,8]
  list_3d(2,:,3) = [9,9,9]
  list_3d(3,:,1) = [10,10,10]
  list_3d(3,:,2) = [11,11,11]
  list_3d(3,:,3) = [12,12,12]

  !Dim1-Access
  write(str1, "(9(1X,I0))") [list_3d(:,1,1), list_3d(:,1,2), list_3d(:,1,3)]
  print "(9(1X,I0))", [list_3d(:,1,1), list_3d(:,1,2), list_3d(:,1,3)]
  if (trim(str1) /= " 4 7 10 5 8 11 6 9 12") error stop
  write(str1, "(9(1X,I0))") [list_3d(:,2,1), list_3d(:,2,2), list_3d(:,2,3)]
  print "(9(1X,I0))", [list_3d(:,2,1), list_3d(:,2,2), list_3d(:,2,3)]
  if (trim(str1) /= " 4 7 10 5 8 11 6 9 12") error stop

  !Dim2-Access
  write(str1, "(9(1X,I0))") [list_3d(1,:,1), list_3d(1,:,2), list_3d(1,:,3)]
  print "(9(1X,I0))", [list_3d(1,:,1), list_3d(1,:,2), list_3d(1,:,3)]
  if (trim(str1) /= " 4 4 4 5 5 5 6 6 6") error stop
  write(str1, "(9(1X,I0))") [list_3d(2,:,1), list_3d(2,:,2), list_3d(2,:,3)]
  print "(9(1X,I0))", [list_3d(2,:,1), list_3d(2,:,2), list_3d(2,:,3)]
  if (trim(str1) /= " 7 7 7 8 8 8 9 9 9") error stop

  !Dim3-Access
  write(str1, "(9(1X,I0))") [list_3d(1,1,:), list_3d(1,2,:), list_3d(1,3,:)]
  print "(9(1X,I0))", [list_3d(1,1,:), list_3d(1,2,:), list_3d(1,3,:)]
  if (trim(str1) /= " 4 5 6 4 5 6 4 5 6") error stop
  write(str1, "(9(1X,I0))") [list_3d(2,1,:), list_3d(2,2,:), list_3d(2,3,:)]
  print "(9(1X,I0))", [list_3d(2,1,:), list_3d(2,2,:), list_3d(2,3,:)]
  if (trim(str1) /= " 7 8 9 7 8 9 7 8 9") error stop

  !2D Access
  write(str1, "(27(1X,I0))") [list_3d(:,:,1), list_3d(:,:,2), list_3d(:,:,3)]
  print "(27(1X,I0))", [list_3d(:,:,1), list_3d(:,:,2), list_3d(:,:,3)]
  if (trim(str1) /= " 4 7 10 4 7 10 4 7 10 5 8 11 5 8 11 5 8 11 6 9 12 6 9 12 6 9 12") error stop
  write(str1, "(27(1X,I0))") [list_3d(:,1,:), list_3d(:,2,:), list_3d(:,3,:)]
  print "(27(1X,I0))", [list_3d(:,1,:), list_3d(:,2,:), list_3d(:,3,:)]
  if (trim(str1) /= " 4 7 10 5 8 11 6 9 12 4 7 10 5 8 11 6 9 12 4 7 10 5 8 11 6 9 12") error stop
  write(str1, "(27(1X,I0))") [list_3d(1,:,:), list_3d(2,:,:), list_3d(3,:,:)]
  print "(27(1X,I0))", [list_3d(1,:,:), list_3d(2,:,:), list_3d(3,:,:)]
  if (trim(str1) /= " 4 4 4 5 5 5 6 6 6 7 7 7 8 8 8 9 9 9 10 10 10 11 11 11 12 12 12") error stop

  !Access a continuous subset of values
  write(str1, "(8(1X,I0))") [list_3d(1:2,1:2,1:2)]
  print "(8(1X,I0))", [list_3d(1:2,1:2,1:2)]
  if (trim(str1) /= " 4 7 4 7 5 8 5 8") error stop
  write(str1, "(12(1X,I0))") [list_3d(2:3,2:3,:)]
  print "(12(1X,I0))", [list_3d(2:3,2:3,:)]
  if (trim(str1) /= " 7 10 7 10 8 11 8 11 9 12 9 12") error stop
  write(str1, "(12(1X,I0))") [list_3d(2:3,1:2,2:3)]
  print "(12(1X,I0))", [list_3d(2:3,1:2,2:3)]
  if (trim(str1) /= " 8 11 8 11 9 12 9 12") error stop

  print *, "All tests passed!"

end program print_arr_08