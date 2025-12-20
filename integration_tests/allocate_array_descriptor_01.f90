program allocate_array_descriptor_01
   implicit none
   
   integer, allocatable :: arr(:)
   integer :: i
   real, allocatable :: matrix(:, :)
   integer :: j
   
   allocate(arr(10))

   do i = 1, 10
      arr(i) = i
   end do
   
   do i = 1, 10
      if (arr(i) /= i) error stop "Array value mismatch"
   end do
   
   deallocate(arr)
   
   allocate(matrix(5, 5))
   
   do i = 1, 5
      do j = 1, 5
         matrix(i, j) = i * 10 + j
      end do
   end do
   
   do i = 1, 5
      do j = 1, 5
         if (abs(matrix(i, j) - (i * 10 + j)) > 1e-10) error stop "Matrix value mismatch"
      end do
   end do
   
   deallocate(matrix)
   
   print *, "All tests passed!"
end program allocate_array_descriptor_01
