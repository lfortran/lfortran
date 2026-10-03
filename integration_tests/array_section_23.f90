program array_section_23
! Test array sections mixing vector subscripts with triplet subscripts.
! Covers rank-2 and rank-3 arrays with non-unit and negative strides.
implicit none

integer :: v2(-2:2, 3:7)
integer :: v3(-1:2, 4:6, 10:13)
integer :: i, j, k

! Initialize v2
do j = 3, 7
   do i = -2, 2
      v2(i,j) = 100*i + j
   end do
end do

! Initialize v3
do k = 10, 13
   do j = 4, 6
      do i = -1, 2
         v3(i,j,k) = 10000*i + 100*j + k
      end do
   end do
end do

call test_rank2()
call test_rank3()

contains

subroutine test_rank2()
   integer :: r2(3,3)

   ! vector subscript on dim1, ascending triplet with stride 2 on dim2
   r2 = v2([0,0,-2], 3:7:2)
   ! Expected columns: v2([0,0,-2], 3), v2([0,0,-2], 5), v2([0,0,-2], 7)
   if (r2(1,1) /= 3)    error stop  ! v2(0,3)
   if (r2(2,1) /= 3)    error stop  ! v2(0,3)
   if (r2(3,1) /= -197) error stop  ! v2(-2,3)
   if (r2(1,2) /= 5)    error stop  ! v2(0,5)
   if (r2(2,2) /= 5)    error stop  ! v2(0,5)
   if (r2(3,2) /= -195) error stop  ! v2(-2,5)
   if (r2(1,3) /= 7)    error stop  ! v2(0,7)
   if (r2(2,3) /= 7)    error stop  ! v2(0,7)
   if (r2(3,3) /= -193) error stop  ! v2(-2,7)
end subroutine

subroutine test_rank3()
   integer :: r3a(2,3,3), r3b(3,3,2)
   integer, parameter :: lb33 = 10, ub33 = 13

   ! triplet with stride 2 on dim1, full range on dim2, vector on dim3
   r3a = v3(-1:2:2, :, [13,10,12])
   ! dim1 indices: [-1, 1], dim2: [4,5,6], dim3: [13,10,12]
   if (r3a(1,1,1) /= v3(-1,4,13)) error stop  ! -10000+400+13 = -9587
   if (r3a(2,1,1) /= v3( 1,4,13)) error stop  !  10000+400+13 = 10413
   if (r3a(1,2,2) /= v3(-1,5,10)) error stop  ! -10000+500+10 = -9490
   if (r3a(2,3,3) /= v3( 1,6,12)) error stop  !  10000+600+12 = 10612

   ! vector on dim1, descending triplet on dim2, descending triplet on dim3
   r3b = v3([2,-1,1], 6:4:-1, ub33:lb33:-2)
   ! dim1: [2,-1,1], dim2: [6,5,4], dim3: [13,11]
   if (r3b(1,1,1) /= v3( 2,6,13)) error stop  !  20000+600+13 = 20613
   if (r3b(2,1,1) /= v3(-1,6,13)) error stop  ! -10000+600+13 = -9387
   if (r3b(3,1,1) /= v3( 1,6,13)) error stop  !  10000+600+13 = 10613
   if (r3b(1,2,1) /= v3( 2,5,13)) error stop  !  20000+500+13 = 20513
   if (r3b(1,3,1) /= v3( 2,4,13)) error stop  !  20000+400+13 = 20413
   if (r3b(1,1,2) /= v3( 2,6,11)) error stop  !  20000+600+11 = 20611
   if (r3b(3,3,2) /= v3( 1,4,11)) error stop  !  10000+400+11 = 10411

   print *, "PASS"
end subroutine

end program
