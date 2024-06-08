module fortran_primes_01_module
   use iso_fortran_env

   implicit none

   integer, parameter :: IP = int32  ! Integer kind parameter for 32-bit integers
   integer, parameter :: WP = int64  ! Integer kind parameter for 64-bit integers
end module fortran_primes_01_module

elemental function witnesses_for_64(n) result(i)
   use fortran_primes_01_module
   implicit none
   integer(WP), intent(in) :: n  ! Input integer with kind parameter WP
   integer(WP) :: i             ! Output integer with kind parameter WP

   ! Compute the number of Miller-Rabin witnesses for n using bitwise XOR, left shift, and multiplication
   ! i = ieor(shifta(n, 32_WP), n) * int(z'45d9f3b3335b369', WP)    TODO: add boz constant support for int intrinsic
   i = ieor(shifta(n, 32_WP), n) * 314582625596846953_WP

end function witnesses_for_64

program fortran_primes_01
   use fortran_primes_01_module
   implicit none
   integer(WP) :: n, i
   
   interface
      function witnesses_for_64(n) result(i)
         use fortran_primes_01_module
         implicit none
         integer(WP), intent(in) :: n
         integer(WP) :: i
      end function witnesses_for_64
   end interface

   n = 2_WP  ! Set n to 2 (with kind parameter WP)

   i = witnesses_for_64(n)
   print *, i
   if (i /= 629165251193693906_WP) error stop

end program
