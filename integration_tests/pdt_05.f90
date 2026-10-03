module pdt_05_mod
   use iso_fortran_env
   implicit none

   type :: parent(ik)
      integer, kind :: ik = int32
      integer(ik) :: a
   end type parent

   type, extends(parent) :: intermediate
      real(real32) :: mid_val
   end type intermediate

   type, extends(intermediate) :: child(rk)
      integer, kind :: rk = real32
      real(rk) :: c
   end type child

end module pdt_05_mod


program pdt_05
   use pdt_05_mod
   use iso_fortran_env
   implicit none

   type(child(ik=int64, rk=real64)) :: obj_full
   type(child(ik=int64)) :: obj_parent_only
   type(child(rk=real64)) :: obj_child_only
   type(child) :: obj_default

   ! ---- obj_full ----
   if (obj_full%ik /= int64) error stop "obj_full ik wrong"
   if (obj_full%rk /= real64) error stop "obj_full rk wrong"

   ! ---- obj_parent_only ----
   if (obj_parent_only%ik /= int64) error stop "obj_parent_only ik wrong"
   if (obj_parent_only%rk /= real32) error stop "obj_parent_only rk default wrong"

   ! ---- obj_child_only ----
   if (obj_child_only%ik /= int32) error stop "obj_child_only ik default wrong"
   if (obj_child_only%rk /= real64) error stop "obj_child_only rk wrong"

   ! ---- obj_default ----
   if (obj_default%ik /= int32) error stop "obj_default ik default wrong"
   if (obj_default%rk /= real32) error stop "obj_default rk default wrong"

   if (storage_size(obj_full%a) /= 64) error stop "obj_full a wrong size"
   if (storage_size(obj_full%c) /= 64) error stop "obj_full c wrong size"

   if (storage_size(obj_parent_only%a) /= 64) error stop "parent_only a wrong size"
   if (storage_size(obj_child_only%c) /= 64) error stop "child_only c wrong size"

   obj_full%a = 2_int64**40
   obj_full%c = 1.0_real64 / 3.0_real64

   if (obj_full%a <= 0_int64) then
      error stop "int64 assignment failed"
   end if

   if (abs(obj_full%c - 0.333333333333d0) > 1.0d-12) then
      error stop "real64 precision lost"
   end if


   print *, "3-layer PDT keyword resolution pdt_05 passed."

end program pdt_05