! The program's own module named generics. It is an ordinary module: USE
! without a module nature, or with NON_INTRINSIC, accesses it, and
! `use, intrinsic :: generics` never does, although generics.mod is visible.
module generics
   implicit none
   private
   public :: INumeric, origin

   abstract interface :: INumeric
      integer
   end interface INumeric

contains

   integer function origin()
      origin = 7
   end function origin
end module generics
