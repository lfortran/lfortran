! Interfaces for gpu_metal_269.  The bodies live in a submodule compiled
! on its own, so the kernel builder has to read them back from the .smod
! file -- which is what the test is about.
module gpu_metal_269_m
   implicit none

   type :: weights_t
      real, allocatable :: pair(:)
   end type weights_t

   interface

      ! An array constructor holding an implied-do whose trip count is
      ! only known at run time.
      pure module function ramp(c) result(f)
         implicit none
         real, intent(in) :: c(:)
         real :: f(size(c))
      end function ramp

      ! The same, with a dot_product over a section of the dummy inside
      ! the implied-do body.
      pure module function paired(self, c) result(f)
         implicit none
         class(weights_t), intent(in) :: self
         real, intent(in) :: c(:)
         real :: f(size(c))
      end function paired

      ! A constant trip count, as a fence.
      pure module function fixed_head(c) result(f)
         implicit none
         real, intent(in) :: c(:)
         real :: f(size(c))
      end function fixed_head

   end interface
end module gpu_metal_269_m
submodule(gpu_metal_269_m) gpu_metal_269_s
   implicit none
contains

   module procedure ramp
      integer :: row
      associate (n => size(c))
         f = [ 2.0*c(1), &
               [(c(row) + 0.5*c(row + 1), row = 2, n - 1)], &
               3.0*c(n) ]
      end associate
   end procedure ramp

   module procedure paired
      integer :: row
      associate (n => size(c), k => size(self%pair))
         f = [ 2.0*c(1), &
               [(dot_product(self%pair, c(row:row + k - 1)), row = 2, n - 1)], &
               3.0*c(n) ]
      end associate
   end procedure paired

   module procedure fixed_head
      integer :: row
      associate (n => size(c))
         f = [ [(10.0*c(row), row = 1, 3)], c(4:n) ]
      end associate
   end procedure fixed_head

end submodule gpu_metal_269_s
