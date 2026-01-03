program intrinsics_213
  ! Which is faster: exp or log?
  integer:: n, k = 2
  real:: x,dx,y,z,t1,t2
  print "(A,I0)",'k = ',k
  dx = 1e-6
  call cpu_time(t1)
  x = 0
  do n = 1,10**k
     x = x+dx
     z = exp(-1/x)
  end do
  call cpu_time(t2)
  print "(2(A,F10.3))",'after 10**k exp, abs(t2-t1) = ',abs(t2-t1),' s; z=',z
  if (abs(z) > 1e-8) error stop
  x = 1
  do n = 1,10**k
     x = x+dx
     z = log(x)
  end do
  call cpu_time(t1)
  print "(2(A,F10.3))",'after 10**k log, abs(t2-t1) = ',abs(t2-t1),' s; z=',z
  if (abs(z - (9.53628842e-05)) > 1e-8) error stop
end program intrinsics_213

