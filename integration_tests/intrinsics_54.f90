program intrinsics_54
   integer :: a, b, c, d, e, f
   a = 2
   b = -3
   c = 5
   d = 10
   e = 20
   f = -30

   if (max0(10,20) /= 20) error stop
   if (max0(3,5,4) /= 5) error stop
   if (max0(a,b) /= a) error stop
   if (max0(a,b,c) /= c) error stop
   if (max0(d,e,f,a,b,c) /= e) error stop

end
