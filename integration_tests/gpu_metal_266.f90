module gpu_metal_266_mod
   implicit none
   type :: op_t
      real, allocatable :: lower_(:,:)
   end type
end module

! A `do concurrent` whose BLOCK-local workspace is sized by
! `size(ops(sel)%lower_, 1)`: the extent of an allocatable array component
! reached through a subscript into an array of derived types. The kernel
! is handed that component as flat data plus a per-element total size, so
! the extent has to become a scalar kernel argument the host computes at
! launch time. Before that, the loop was declined and ran on the host.
program gpu_metal_266
   use gpu_metal_266_mod
   implicit none
   integer, parameter :: n = 4, nc = 3, sel = 2
   type(op_t), allocatable :: ops(:)
   real :: a(n,n), x(n,nc), y(n,nc)
   integer :: i, j

   allocate(ops(2))
   allocate(ops(1)%lower_(1,1))
   allocate(ops(sel)%lower_(n,n))
   ops(1)%lower_ = 0.0
   ops(sel)%lower_ = 0.0

   a = 0.0
   do i = 1, n
      a(i,i) = 2.0
   end do
   do j = 1, nc
      do i = 1, n
         x(i,j) = real(i + j)
      end do
   end do
   y = 0.0

   do concurrent (j = 1:nc)
      block
         real :: tmp(size(ops(sel)%lower_,1))
         integer :: ii, kk
         do ii = 1, n
            tmp(ii) = 0.0
            do kk = 1, n
               tmp(ii) = tmp(ii) + a(ii,kk)*x(kk,j)
            end do
         end do
         do ii = 1, n
            y(ii,j) = tmp(ii)
         end do
      end block
   end do

   do j = 1, nc
      do i = 1, n
         print *, i, j, y(i,j)
         if (abs(y(i,j) - 2.0*x(i,j)) > 1.0e-5) error stop "wrong value"
      end do
   end do
   print *, "ok"
end program
