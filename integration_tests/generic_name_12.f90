module generic_name_12_mod
   ! A generic interface whose only specific procedure has the same name, with
   ! the specific called from *inside* the same module. The specific is stored
   ! under a disambiguated symbol table key while keeping its own name, so
   ! resolving the call by name finds the GenericProcedure instead of the
   ! procedure and ASR verification rejects the call. toml-f's terminal.f90
   ! (`interface escape / module procedure :: escape`, used by `//`) is the
   ! real-world case; it reaches LFortran through fpm.
   implicit none
   private
   public :: escape, concat

   interface escape
      module procedure :: escape
   end interface escape

contains

   pure function escape(code) result(str)
      integer, intent(in) :: code
      character(len=1) :: str
      str = achar(64 + code)
   end function escape

   pure function concat(lval, code) result(str)
      character(len=*), intent(in) :: lval
      integer, intent(in) :: code
      character(len=len(lval)+1) :: str
      str = lval // escape(code)
   end function concat

end module generic_name_12_mod

program generic_name_12
   use generic_name_12_mod, only: escape, concat
   implicit none
   if (escape(1) /= "A") error stop
   if (concat("x", 2) /= "xB") error stop
   print *, "ok"
end program generic_name_12
