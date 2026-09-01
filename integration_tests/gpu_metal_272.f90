! An allocatable array component of a kernel argument that is an array of
! derived type is handed to the device as three buffers -- the
! concatenated element data, the per-element offsets into it and the
! per-element sizes.  The sizes buffer carried one number per element,
! the element's total count, which is all a rank-one component needs.
!
! A component of rank two or more needs more than that.  Its data is
! flattened contiguously, so `c(i)%m_(p,q)` is at
! `offsets[i] + (p-1) + (q-1)*extent1`, and nothing on the device knew
! `extent1`.  The Metal code generator emitted `offsets[i] + 0` for any
! subscript list longer than one, so every thread read element (1,1) of
! the component; a section `c(i)%m_(:,j)` was worse, since the host also
! passed the component's total count where the section wanted the extent
! of one dimension.  Neither was diagnosed: the shader compiled and the
! program printed shifted, cross-contaminated numbers.
!
! The host under-copied as well.  It sized the flat buffer from
! `get_array_size` of the component's descriptor, which returned the
! extent of dimension one rather than the product of the extents, so a
! 3x4 component reached the device as its first three elements with the
! rest reading off the end of the allocation.
!
! The sizes buffer now holds the *extents* of the component, one entry
! per dimension per element, so element `k` of a rank `R` component
! occupies entries `k*R .. k*R+R-1`.  A rank-one component keeps exactly
! the one entry per element it always had; a higher rank gets its total
! back as the product.  The three emitters and the host agree on that
! layout through `gpu_struct_member_sizes_name` in `gpu_utils.h`.
!
! All four subroutines offload, so this program emits exactly 4 kernels.
module gpu_metal_272_mod
   implicit none

   type :: leaf_t
      real, allocatable :: m_(:,:)
      real, allocatable :: v_(:)
      real, allocatable :: t_(:,:,:)
   end type

contains

   ! Elementwise read of a rank-2 component.  Every (i,j) is distinct, so
   ! a kernel that linearizes the subscripts wrongly cannot pass.
   subroutine run_elem(o, c, nn, mm)
      real, intent(out) :: o(:,:)
      type(leaf_t), intent(in) :: c(:)
      integer, intent(in) :: nn, mm
      integer :: j, i
      do concurrent (j = 1:nn)
         do i = 1, mm
            o(i, j) = c(1)%m_(i, j)
         end do
      end do
   end subroutine

   ! A section of a rank-2 component.  The section length is the extent
   ! of dimension 1, not the component's element count.
   subroutine run_section(o, c, nn)
      real, intent(out) :: o(:,:)
      type(leaf_t), intent(in) :: c(:)
      integer, intent(in) :: nn
      integer :: j
      do concurrent (j = 1:nn)
         o(:, j) = c(1)%m_(:, j)
      end do
   end subroutine

   ! A rank-3 component beside the rank-1 fence: `v_` has always worked
   ! and must keep working, since a rank-1 component still occupies one
   ! entry per element of the sizes buffer.
   subroutine run_rank3(o, c, nn)
      real, intent(out) :: o(:)
      type(leaf_t), intent(in) :: c(:)
      integer, intent(in) :: nn
      integer :: j
      do concurrent (j = 1:nn)
         o(j) = c(1)%t_(2, 3, j) + c(1)%v_(j)
      end do
   end subroutine

   ! The rank-2 component passed whole to a device routine with an
   ! assumed-shape dummy: the call site has to hand over the extent of
   ! each dimension, and used to pass the element count for both.
   pure function column_sum(a, j) result(res)
      real, intent(in) :: a(:,:)
      integer, intent(in) :: j
      real :: res
      integer :: k
      res = 0.0
      do k = 1, size(a, 1)
         res = res + a(k, j)
      end do
   end function

   subroutine run_device_fn(o, c, nn)
      real, intent(out) :: o(:)
      type(leaf_t), intent(in) :: c(:)
      integer, intent(in) :: nn
      integer :: j
      do concurrent (j = 1:nn)
         o(j) = column_sum(c(1)%m_, j)
      end do
   end subroutine

end module

program gpu_metal_272
   use gpu_metal_272_mod
   implicit none

   type(leaf_t), allocatable :: c(:)
   real :: o(3,4), os(3,4), o3(4), od(4)
   real :: expected
   integer :: i, j, k

   allocate(c(1))
   allocate(c(1)%m_(3,4))
   allocate(c(1)%v_(4))
   allocate(c(1)%t_(2,3,4))
   do j = 1, 4
      do i = 1, 3
         c(1)%m_(i,j) = real(10*j + i)
      end do
      c(1)%v_(j) = real(j)
      do k = 1, 3
         do i = 1, 2
            c(1)%t_(i,k,j) = real(100*j + 10*k + i)
         end do
      end do
   end do

   o = -1.0
   call run_elem(o, c, 4, 3)
   print *, o
   do j = 1, 4
      do i = 1, 3
         if (abs(o(i,j) - real(10*j + i)) > 1.0e-4) then
            error stop "rank-2 component read elementwise"
         end if
      end do
   end do

   os = -1.0
   call run_section(os, c, 4)
   print *, os
   do j = 1, 4
      do i = 1, 3
         if (abs(os(i,j) - real(10*j + i)) > 1.0e-4) then
            error stop "section of a rank-2 component"
         end if
      end do
   end do

   o3 = -1.0
   call run_rank3(o3, c, 4)
   print *, o3
   do j = 1, 4
      if (abs(o3(j) - (real(100*j + 32) + real(j))) > 1.0e-4) then
         error stop "rank-3 component and rank-1 fence"
      end if
   end do

   od = -1.0
   call run_device_fn(od, c, 4)
   print *, od
   do j = 1, 4
      expected = 0.0
      do i = 1, 3
         expected = expected + real(10*j + i)
      end do
      if (abs(od(j) - expected) > 1.0e-4) then
         error stop "rank-2 component passed to a device routine"
      end if
   end do

   print *, "ok"
end program
