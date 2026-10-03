module M_framework__verify
   implicit none
   private

   logical, save :: G_debug = .false.

   public :: cmdline_, get_debug

contains

   subroutine cmdline_(filename)
      character(len=*), intent(in) :: filename
      namelist /args/ G_debug
      integer :: unit

      open(newunit=unit, file=filename, status="old", action="read")
      read(unit, nml=args)
      close(unit)
   end subroutine cmdline_

   function get_debug() result(val)
      logical :: val
      val = G_debug
   end function get_debug

end module M_framework__verify


program test_verify
   use M_framework__verify
   implicit none

   integer :: unit
   character(len=*), parameter :: fname = "input.nml"

! Create the namelist file automatically
   open(newunit=unit, file=fname, status="replace", action="write")
   write(unit,*) "&args"
   write(unit,*) "G_debug = .true."
   write(unit,*) "/"
   close(unit)

! Read the namelist
   call cmdline_(fname)

! Verify result
   print *, "Debug mode =", get_debug()

end program test_verify
