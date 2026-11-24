! A complex literal in a DATA value list may use a named constant as its
! real or imaginary part (Fortran 2023 R719-R720, C723).
module data_30_mod
   implicit none
   real, parameter :: a = 2.0
   integer, parameter :: n = 3
   complex :: z(4)
   data z / (a, 0.0), (0.0, a), (a, -1.0), (n, 0) /
end module data_30_mod

program data_30
   use data_30_mod
   implicit none
   if (z(1) /= cmplx(2.0, 0.0)) error stop 1
   if (z(2) /= cmplx(0.0, 2.0)) error stop 2
   if (z(3) /= cmplx(2.0, -1.0)) error stop 3
   if (z(4) /= cmplx(3.0, 0.0)) error stop 4
end program data_30
