module data_implied_do_09_mod
   integer, parameter :: nc = 1
   real(8), dimension(nc,2), target :: coeff
   integer :: n
   data ( coeff(n,1),n=1,1 ) /1.0d0/
end module data_implied_do_09_mod

program data_implied_do_09
   use data_implied_do_09_mod
   implicit none
   print *, coeff(1,1)
   if (coeff(1,1) /= 1.0d0) error stop
end program data_implied_do_09
