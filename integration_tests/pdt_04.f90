module pdt_04_mod
   use iso_fortran_env
   implicit none

   type :: base(ik)
      integer, kind :: ik = int32
      integer(ik) :: a
   end type base

   type, extends(base) :: mid(rk)
      integer, kind :: rk = real32
      real(rk) :: b
   end type mid

   type, extends(mid) :: leaf(ck)
      integer, kind :: ck = int8
      integer(ck) :: c
   end type leaf

end module pdt_04_mod


program pdt_04
   use pdt_04_mod
   use iso_fortran_env
   implicit none

   type(leaf) :: d0                    ! all defaults
   type(leaf(ck=int16)) :: d1          ! override leaf only
   type(leaf(ik=int64)) :: d2          ! override base only
   type(leaf(int64, real64, int32)) :: d3 ! positional

   ! Default check
   if (d0%ik /= int32) error stop "default ik wrong"
   if (d0%rk /= real32) error stop "default rk wrong"
   if (d0%ck /= int8)   error stop "default ck wrong"

   ! Leaf-only override
   if (d1%ck /= int16) error stop "ck override failed"
   if (d1%ik /= int32) error stop "ik changed unexpectedly"

   ! Base-only override
   if (d2%ik /= int64) error stop "ik override failed"
   if (storage_size(d2%a) /= 64) error stop "storage mismatch"

   ! Positional
   if (d3%ik /= int64) error stop "pos ik wrong"
   if (d3%rk /= real64) error stop "pos rk wrong"
   if (d3%ck /= int32) error stop "pos ck wrong"

   print *, "MRE 2 passed"
end program pdt_04