program gpu_metal_260
   ! `matmul` inlined into a Metal kernel indexed an operand that is an
   ! array section by the contraction loop variable, which runs over the
   ! *other* operand's index space.  The section's own start was dropped,
   ! so matmul(A, v(6:9)) read v(1:4).
   implicit none

   integer, parameter :: nrow = 3, ncol = 4, nv = 12, nj = 5
   real :: A(nrow, ncol), B(ncol, nrow), M2(6, ncol)
   real :: v(nv), w(0:ncol-1)
   real :: c1(nrow, nj), c2(nrow, nj), c4(nrow, nj)
   real :: c3(nrow, ncol, nj)
   real :: e1(nrow), e2(nrow), e3(nrow, ncol), e4(nrow)
   integer :: i, j, k

   do i = 1, nrow
      do k = 1, ncol
         A(i,k) = real(i) + 10.0*real(k)
         B(k,i) = real(k) - 2.0*real(i)
      end do
   end do
   do i = 1, 6
      do k = 1, ncol
         M2(i,k) = real(i)*0.5 - real(k)
      end do
   end do
   do i = 1, nv
      v(i) = real(i)*real(i)
   end do
   do i = 0, ncol-1
      w(i) = 100.0 + real(i)
   end do

   ! Reference values, computed on the host.
   e1 = matmul(A, v(6:9))
   e2 = matmul(v(6:9), B)
   e3 = matmul(A, M2(3:6, :))
   e4 = matmul(A, w)

   c1 = 0.; c2 = 0.; c3 = 0.; c4 = 0.

   ! (m,k) x (k) with a vector operand that is a section starting at 6.
   do concurrent (j = 1:nj)
      c1(:,j) = matmul(A, v(6:9)) * real(j)
   end do

   ! (k) x (k,n) with the same section as the left operand.
   do concurrent (j = 1:nj)
      c2(:,j) = matmul(v(6:9), B)
   end do

   ! (m,k) x (k,n) with a right operand whose rows are a section.
   do concurrent (j = 1:nj)
      c3(:,:,j) = matmul(A, M2(3:6, :))
   end do

   ! (m,k) x (k) with a whole array whose lower bound is 0, not 1.
   do concurrent (j = 1:nj)
      c4(:,j) = matmul(A, w)
   end do

   do j = 1, nj
      do i = 1, nrow
         if (abs(c1(i,j) - e1(i)*real(j)) > 1e-4*max(1.0, abs(e1(i)*real(j)))) then
            print *, "c1", i, j, c1(i,j), e1(i)*real(j)
            error stop "c1"
         end if
         if (abs(c2(i,j) - e2(i)) > 1e-4*max(1.0, abs(e2(i)))) then
            print *, "c2", i, j, c2(i,j), e2(i)
            error stop "c2"
         end if
         if (abs(c4(i,j) - e4(i)) > 1e-4*max(1.0, abs(e4(i)))) then
            print *, "c4", i, j, c4(i,j), e4(i)
            error stop "c4"
         end if
         do k = 1, ncol
            if (abs(c3(i,k,j) - e3(i,k)) > 1e-4*max(1.0, abs(e3(i,k)))) then
               print *, "c3", i, k, j, c3(i,k,j), e3(i,k)
               error stop "c3"
            end if
         end do
      end do
   end do

   print *, "ok"

end program
