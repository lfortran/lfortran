program count_param_01
  implicit none
  integer, parameter :: iarray(4) = [1,2,4,4], nmax = size(iarray), &
       n = nmax - count(iarray(1:nmax-1)==iarray(2:nmax))
  if (nmax /= 4) error stop
  if (n /= 3) error stop
  print "(2(A,1X,I0),*(1X,I0))", 'n =', n, ' iarray =', iarray
end program count_param_01
