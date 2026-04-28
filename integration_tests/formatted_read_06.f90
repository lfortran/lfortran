program formatted_read_06
   ! Test B/O/Z formatted READ descriptors (binary, octal, hexadecimal).
   implicit none
   integer :: ierr, intg
   integer :: ints(3)

   ! Binary
   read('111', '(B3)', iostat=ierr) intg
   if (ierr /= 0) error stop 1
   if (intg /= 7) error stop 2

   read('1010', '(B4)', iostat=ierr) intg
   if (ierr /= 0) error stop 3
   if (intg /= 10) error stop 4

   ! Octal
   read('111', '(O3)', iostat=ierr) intg
   if (ierr /= 0) error stop 5
   if (intg /= 73) error stop 6

   read('17', '(O2)', iostat=ierr) intg
   if (ierr /= 0) error stop 7
   if (intg /= 15) error stop 8

   ! Hexadecimal
   read('111', '(Z3)', iostat=ierr) intg
   if (ierr /= 0) error stop 9
   if (intg /= 273) error stop 10

   read('FF', '(Z2)', iostat=ierr) intg
   if (ierr /= 0) error stop 11
   if (intg /= 255) error stop 12

   read('ab', '(Z2)', iostat=ierr) intg
   if (ierr /= 0) error stop 13
   if (intg /= 171) error stop 14

   ! Width larger than significant digits (trailing blanks treated as nulls by default).
   read('111   ', '(B6)', iostat=ierr) intg
   if (ierr /= 0) error stop 15
   if (intg /= 7) error stop 16

   ! Reading into an array via repeat count.
   read('010111', '(3B2)', iostat=ierr) ints
   if (ierr /= 0) error stop 17
   if (ints(1) /= 1) error stop 18
   if (ints(2) /= 1) error stop 19
   if (ints(3) /= 3) error stop 20

   print *, "OK"
end program
