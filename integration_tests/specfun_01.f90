subroutine rcty_(n,x)
   implicit double precision (a-h,o-z)
   dimension ry(0:n),dy(0:n)
   nm=n
   if (x.lt.1.0d-60) then
      do 10 k=0,n
         ry(k)=-1.0d+300
10    dy(k)=1.0d+300
      ry(0)=-1.0d0
      dy(0)=0.0d0
      return
   endif
   ry(0)=-dcos(x)
   ry(1)=ry(0)/x-dsin(x)
   rf0=ry(0)
   rf1=ry(1)
   do 15 k=2,n
      rf2=(2.0d0*k-1.0d0)*rf1/x-rf0
      if (dabs(rf2).gt.1.0d+300) go to 20
      ry(k)=rf2
      rf0=rf1
15 rf1=rf2
20 nm=k-1
   dy(0)=dsin(x)
   do 25 k=1,nm
25 dy(k)=-k*ry(k)/x+ry(k-1)

   if (abs(ry(0) - (-0.98006657784124163)) > 1e-7) error stop
   if (abs(ry(1) - (-5.0990022200012692)) > 1e-6) error stop
   if (abs(ry(2) - (-75.504966722177798)) > 1e-6) error stop
   if (abs(dy(0) - (0.19866933079506122)) > 1e-8) error stop
   if (abs(dy(1) - (24.514944522165102)) > 1e-6) error stop
   if (abs(dy(2) - (749.95066500177666)) > 1e-4) error stop
   if (nm /= 2) error stop

   print *, "ry = ", ry
   print *, "dy = ", dy
   print *, "nm = ", nm
   return
end

program specfun_01
   implicit double precision (a-h,o-z)
   call rcty_(2, 0.2d0)
end program

