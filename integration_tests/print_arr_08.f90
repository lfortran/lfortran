!Non-Unit Stride Access in Print Statements
!Testing various array section prints for 2D and 3D arrays, 
!including multi-dimensional slices and complete array prints.

program print_arr_08
  implicit none
  integer :: list_2d(2,3)
  integer :: list_3d(3,3,3)

  !Test 2D Array Section
  !Assign Row Values
  list_2d(1,:) = [2,2,2]
  list_2d(2,:) = [3,3,3]
  !Row-Access
  print "(3(1X,I0))", list_2d(1,:), list_2d(2,:)
  !Column-Access
  print "(3(1X,I0))", list_2d(:,1),list_2d(:,2),list_2d(:,3)

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
  print "(3(1X,I0))", list_3d(:,1,1), list_3d(:,1,2), list_3d(:,1,3)
  print "(3(1X,I0))", list_3d(:,2,1), list_3d(:,2,2), list_3d(:,2,3)
  print "(3(1X,I0))", list_3d(:,3,1), list_3d(:,3,2), list_3d(:,3,3)
  
  !Dim2-Access
  print "(3(1X,I0))", list_3d(1,:,1), list_3d(1,:,2), list_3d(1,:,3)
  print "(3(1X,I0))", list_3d(2,:,1), list_3d(2,:,2), list_3d(2,:,3)
  print "(3(1X,I0))", list_3d(3,:,1), list_3d(3,:,2), list_3d(3,:,3)
  
  !Dim3-Access
  print "(3(1X,I0))", list_3d(1,1,:), list_3d(1,2,:), list_3d(1,3,:)
  print "(3(1X,I0))", list_3d(2,1,:), list_3d(2,2,:), list_3d(2,3,:)
  print "(3(1X,I0))", list_3d(3,1,:), list_3d(3,2,:), list_3d(3,3,:)

  !2D Access
  print "(3(1X,I0))", list_3d(:,:,1), list_3d(:,:,2), list_3d(:,:,3)
  print "(3(1X,I0))", list_3d(:,1,:), list_3d(:,2,:), list_3d(:,3,:)
  print "(3(1X,I0))", list_3d(1,:,:), list_3d(2,:,:), list_3d(3,:,:)

  !Access a continous subset of values
  print "(3(1X,I0))", list_3d(1:2,1:2,1:2)
  print "(3(1X,I0))", list_3d(2:3,2:3,:)
  print "(3(1X,I0))", list_3d(2:3,1:2,2:3)

end program print_arr_08