! An element of an array of derived type reached through a component --
! `g%points_(k,1,1,1)` -- is copied into a scalar temporary on the host
! before the launch, because nothing on the device carries the extents of
! the array components hanging off the selected element.
!
! When the loop WRITES into that element the copy is not enough on its
! own: a read-only copy would be thrown away and the write lost.  The
! element is instead copied back over the original once the kernel has
! finished.  Because the copy in brought the whole element across first,
! the copy back is exact even when the kernel wrote only part of it.
!
! It is only sound while every write to the object lands inside that one
! element, so a loop that also writes a sibling component, or a second
! element of the same object, is left on the host.
module gpu_metal_285_m
   implicit none

   type :: point_t
      real, allocatable :: values_(:,:)
   end type point_t

   type :: field_t
      integer :: tag_ = 0
      type(point_t), allocatable :: points_(:,:,:,:)
   end type field_t

contains

   ! The shape the fix targets: the gathered element is the assignment
   ! target, and every column is written.
   subroutine scale_all(self, out, w)
      type(field_t), intent(in) :: self
      type(field_t), intent(inout) :: out
      real, intent(in) :: w
      integer :: j
      do concurrent (j = 1:size(self%points_(1,1,1,1)%values_, 2))
         out%points_(1,1,1,1)%values_(:,j) = &
            w*self%points_(1,1,1,1)%values_(:,j)
      end do
   end subroutine scale_all

   ! Only part of the element is written; the rest has to survive the
   ! copy back untouched.
   subroutine scale_first_two(self, out, w)
      type(field_t), intent(in) :: self
      type(field_t), intent(inout) :: out
      real, intent(in) :: w
      integer :: j
      do concurrent (j = 1:2)
         out%points_(1,1,1,1)%values_(:,j) = &
            w*self%points_(1,1,1,1)%values_(:,j)
      end do
   end subroutine scale_first_two

   ! A read-only chain: the fence.  Nothing here is written, so this must
   ! keep offloading exactly as it did before.
   subroutine read_only(self, r, w)
      type(field_t), intent(in) :: self
      real, intent(out) :: r(:,:)
      real, intent(in) :: w
      integer :: j
      do concurrent (j = 1:size(self%points_(1,1,1,1)%values_, 2))
         r(:,j) = w*self%points_(1,1,1,1)%values_(:,j)
      end do
   end subroutine read_only

   ! Writes a component of the object that is outside the gathered
   ! element, so the copy back would undo it.  Declined; the values still
   ! have to come out right on the host.
   subroutine scale_and_tag(self, out, w)
      type(field_t), intent(in) :: self
      type(field_t), intent(inout) :: out
      real, intent(in) :: w
      integer :: j
      do concurrent (j = 1:size(self%points_(1,1,1,1)%values_, 2))
         out%points_(1,1,1,1)%values_(:,j) = &
            w*self%points_(1,1,1,1)%values_(:,j)
         out%tag_ = j
      end do
   end subroutine scale_and_tag

   ! Two elements of the same object are written, and they could name the
   ! same storage.  Declined; the values still have to come out right.
   subroutine scale_two_elements(self, out, w)
      type(field_t), intent(in) :: self
      type(field_t), intent(inout) :: out
      real, intent(in) :: w
      integer :: j
      do concurrent (j = 1:size(self%points_(1,1,1,1)%values_, 2))
         out%points_(1,1,1,1)%values_(:,j) = &
            w*self%points_(1,1,1,1)%values_(:,j)
         out%points_(2,1,1,1)%values_(:,j) = &
            (w + 1.0)*self%points_(1,1,1,1)%values_(:,j)
      end do
   end subroutine scale_two_elements

end module gpu_metal_285_m

program gpu_metal_285
   use gpu_metal_285_m
   implicit none
   integer, parameter :: nx = 3, ny = 4
   type(field_t) :: a, b
   real :: r(nx, ny)
   real :: w
   integer :: i, j, pass

   allocate(a%points_(2,1,1,1))
   allocate(a%points_(1,1,1,1)%values_(nx, ny))
   allocate(a%points_(2,1,1,1)%values_(nx, ny))
   allocate(b%points_(2,1,1,1))
   allocate(b%points_(1,1,1,1)%values_(nx, ny))
   allocate(b%points_(2,1,1,1)%values_(nx, ny))
   a%points_(2,1,1,1)%values_ = 0.0

   do j = 1, ny
      do i = 1, nx
         a%points_(1,1,1,1)%values_(i,j) = real(i + 10*j)
      end do
   end do

   ! Several passes with distinct weights: a stale copy or a lost write
   ! shows up as the value of an earlier pass.
   do pass = 1, 3
      w = real(pass) + 0.5

      b%points_(1,1,1,1)%values_ = -1.0
      call scale_all(a, b, w)
      do j = 1, ny
         print *, b%points_(1,1,1,1)%values_(:,j)
         do i = 1, nx
            if (abs(b%points_(1,1,1,1)%values_(i,j) &
                  - w*real(i + 10*j)) > 1.0e-4) error stop "scale_all"
         end do
      end do

      b%points_(1,1,1,1)%values_ = -1.0
      call scale_first_two(a, b, w)
      do j = 1, 2
         do i = 1, nx
            if (abs(b%points_(1,1,1,1)%values_(i,j) &
                  - w*real(i + 10*j)) > 1.0e-4) error stop "partial written"
         end do
      end do
      do j = 3, ny
         do i = 1, nx
            if (abs(b%points_(1,1,1,1)%values_(i,j) + 1.0) > 1.0e-4) then
               error stop "partial untouched"
            end if
         end do
      end do

      r = 0.0
      call read_only(a, r, w)
      do j = 1, ny
         do i = 1, nx
            if (abs(r(i,j) - w*real(i + 10*j)) > 1.0e-4) then
               error stop "read_only"
            end if
         end do
      end do

      b%points_(1,1,1,1)%values_ = -1.0
      b%tag_ = 0
      call scale_and_tag(a, b, w)
      do j = 1, ny
         do i = 1, nx
            if (abs(b%points_(1,1,1,1)%values_(i,j) &
                  - w*real(i + 10*j)) > 1.0e-4) error stop "scale_and_tag"
         end do
      end do
      if (b%tag_ < 1 .or. b%tag_ > ny) error stop "tag"

      b%points_(1,1,1,1)%values_ = -1.0
      b%points_(2,1,1,1)%values_ = -1.0
      call scale_two_elements(a, b, w)
      do j = 1, ny
         do i = 1, nx
            if (abs(b%points_(1,1,1,1)%values_(i,j) &
                  - w*real(i + 10*j)) > 1.0e-4) error stop "two first"
            if (abs(b%points_(2,1,1,1)%values_(i,j) &
                  - (w + 1.0)*real(i + 10*j)) > 1.0e-4) then
               error stop "two second"
            end if
         end do
      end do
   end do

   print *, "ok"
end program gpu_metal_285
