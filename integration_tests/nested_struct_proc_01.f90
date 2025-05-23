module gFTL2_Integer32Complex32OrderedMap
   use iso_fortran_env
   implicit none

   integer, parameter :: I32 = int32
   integer, parameter :: R32 = real32

   type :: omap_I_Set
   contains
   end type omap_I_Set

   type :: omap_Map
      private
      type(omap_I_Set) :: tree
      complex(kind=R32) :: stored_value  ! Module-level target
   contains
      procedure :: at_rc => omap_I_at_rc
      generic :: at => at_rc
   end type omap_Map

   type :: Integer32Complex32OrderedMap
      private
      type(omap_Map) :: map
   contains
      procedure :: insert_key_value => omap_insert_key_value
      generic :: insert => insert_key_value
   end type Integer32Complex32OrderedMap

contains

   function omap_I_at_rc(this, key, rc) result(res)
      class(omap_Map), target, intent(in) :: this
      integer(kind=I32), intent(in) :: key
      integer, intent(out) :: rc
      complex(kind=R32), pointer :: res
      res => this%stored_value  ! Point to module-level target
      rc = 0
   end function omap_I_at_rc

   recursive subroutine omap_insert_key_value(this, key, value)
      class(Integer32Complex32OrderedMap), intent(inout) :: this
      integer(kind=I32), intent(in) :: key
      complex(kind=R32), intent(in) :: value
      integer :: status
      complex(kind=R32), pointer :: tmp
      real, parameter :: tol = 1.0e-6  ! Tolerance for comparison

      this%map%stored_value = value  ! Store the value
      tmp => this%map%at(key, rc=status)
      ! print *, "Real:", real(tmp), "Imag:", aimag(tmp), "Status:", status
      if (abs(real(tmp) - real(value)) > tol .or. abs(aimag(tmp) - aimag(value)) > tol) then
         print *, "Test failed: tmp does not match value"
         error stop
      end if
   end subroutine omap_insert_key_value
end module gFTL2_Integer32Complex32OrderedMap

program nested_struct_proc_01
   use gFTL2_Integer32Complex32OrderedMap
   implicit none

   type(Integer32Complex32OrderedMap) :: my_map
   integer(kind=I32) :: key
   complex(kind=R32) :: value

   key = 10
   value = (2.0_R32, 4.0_R32)
   call my_map%insert(key, value)
   print *, "Test passed"
end program nested_struct_proc_01