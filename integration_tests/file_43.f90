program file_43
   implicit none

   integer :: n

   integer :: i
   real    :: r
   logical :: l
   integer :: iarr(5)
   real    :: rarr(3,2)

   print *, "IOLENGTH tests:"

   inquire(iolength=n) i
   print *, "integer        :", n
   if (n /= 4) error stop

   inquire(iolength=n) r
   print *, "real           :", n
   if (n /= 4) error stop

   inquire(iolength=n) l
   print *, "logical        :", n
   if (n /= 4) error stop

   inquire(iolength=n) iarr
   print *, "integer(5)     :", n
   if (n /= 20) error stop

   inquire(iolength=n) rarr
   print *, "real(3x2)      :", n
   if (n /= 24) error stop

end program
