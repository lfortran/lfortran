! Interfaces for gpu_metal_284.  The body lives in a submodule so the
! kernel builder splices a callee it read back from the `.smod` file,
! which is the shape the bug was found in.
module gpu_metal_284_m
   implicit none

   type :: tile_t
      real, allocatable :: values_(:,:)
   end type tile_t

   type :: grid_t
      type(tile_t), allocatable :: tiles_(:)
   end type grid_t

   interface

      ! Sections its own dummy, so a sectioned actual has to be gathered
      ! into a contiguous array of the spliced block first.
      pure module function shifted(w, arr) result(r)
         implicit none
         real, intent(in) :: w
         real, intent(in) :: arr(:)
         real :: r(size(arr))
      end function shifted

   end interface
end module gpu_metal_284_m
submodule(gpu_metal_284_m) gpu_metal_284_s
   implicit none
contains

   module procedure shifted
      integer :: i, n
      real :: t(size(arr))
      real :: cw(2)
      n = size(arr)
      cw(1) = w
      cw(2) = 1.0
      t = 0.0
      ! A section of the dummy: this is what forces the gather.
      t(1:n - 1) = arr(2:n)
      ! A `do concurrent` of the callee's own, holding a dot_product over
      ! a section of the dummy.
      do concurrent (i = 1:n - 1)
         r(i) = dot_product(cw, arr(i:i + 1))
      end do
      r(n) = w*arr(n) + t(n)
   end procedure shifted

end submodule gpu_metal_284_s
