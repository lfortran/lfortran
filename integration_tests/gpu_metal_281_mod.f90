! Interfaces for gpu_metal_281.  The body lives in a submodule compiled on
! its own, so the kernel builder reads it back from the `.smod` file with
! none of this translation unit's constant folding applied to it -- which
! is what leaves a reference to a named constant standing in the ASR the
! Metal code generator is handed.
module gpu_metal_281_m
   implicit none

   interface
      pure module function shift(c) result(f)
         implicit none
         real, intent(in) :: c(:)
         real :: f(size(c))
      end function shift
   end interface

end module gpu_metal_281_m
submodule(gpu_metal_281_m) gpu_metal_281_s
   implicit none
contains

   ! `bump` is a named constant of this procedure.  The local automatic
   ! array `w` is what makes the procedure one that has to be spliced into
   ! the kernel rather than emitted as a device function, so `bump` is
   ! declared in the block the splice creates.
   module procedure shift
      integer, parameter :: bump = 3
      real :: w(size(c))
      integer :: p
      w = c
      do p = 1, bump - 1
         f(p) = w(p)
      end do
      do p = bump, size(c)
         f(p) = 2.0*w(p) + real(size(c) - bump)
      end do
   end procedure shift

end submodule gpu_metal_281_s
