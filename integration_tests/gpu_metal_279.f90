! An allocatable array component inherited from a parent type is reached
! through the parent field rather than by a single field index, so the host
! used to marshal no component buffers at all for a type that extends
! another.  The kernel then indexed the descriptor word of the component
! itself: a rank-1 component read the wrong memory silently, and a rank-2
! one had no dimension-1 extent and failed the build outright.
module gpu_metal_279_m
   implicit none
   type, abstract :: gpu_metal_279_base_t
      integer :: tag_
      real, allocatable :: upper_(:,:)
      real, allocatable :: inner_(:)
   end type gpu_metal_279_base_t
   type, extends(gpu_metal_279_base_t) :: gpu_metal_279_kid_t
   end type gpu_metal_279_kid_t
end module gpu_metal_279_m

program gpu_metal_279
   use gpu_metal_279_m
   implicit none
   integer, parameter :: rows = 3, cols = 4
   type(gpu_metal_279_kid_t) :: op(2)
   real :: elem(cols), sect(rows, cols), fence(cols)
   integer :: i, j

   allocate(op(1)%upper_(rows, cols))
   allocate(op(1)%inner_(cols))
   allocate(op(2)%upper_(1, 1))
   allocate(op(2)%inner_(1))
   op(1)%tag_ = 1
   op(2)%tag_ = 2
   do j = 1, cols
      do i = 1, rows
         op(1)%upper_(i, j) = real(i + 10*j)
      end do
      op(1)%inner_(j) = real(100*j)
   end do
   op(2)%upper_ = -1
   op(2)%inner_ = -1

   elem = 0
   sect = 0
   fence = 0
   ! A rank-2 inherited component read elementwise and as a whole column,
   ! and a rank-1 inherited component as the fence.  The second element of
   ! `op` is deliberately shaped differently so a per-element extent that
   ! came from the wrong element would show up as a wrong number.
   do concurrent (j = 1:cols)
      elem(j) = op(1)%upper_(2, j)
      sect(:, j) = op(1)%upper_(:, j)
      fence(j) = op(1)%inner_(j)
   end do

   do j = 1, cols
      print *, elem(j), sect(:, j), fence(j)
      if (abs(elem(j) - real(2 + 10*j)) > 1.0e-5) error stop "elem"
      do i = 1, rows
         if (abs(sect(i, j) - real(i + 10*j)) > 1.0e-5) error stop "section"
      end do
      if (abs(fence(j) - real(100*j)) > 1.0e-5) error stop "fence"
   end do
end program gpu_metal_279
