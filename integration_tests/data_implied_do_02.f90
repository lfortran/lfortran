program data_implied_do_02

   integer i
   double precision coef(5,4)

   data (coef(i,1),i=1,5)/1.0D0,1.0D0,3*0.0D0/

   if (abs(coef(1,1) - 1.0D0) > 1e-12) error stop
   if (abs(coef(2,1) - 1.0D0) > 1e-12) error stop
   if (abs(coef(3,1) - 0.0D0) > 1e-12) error stop
   if (abs(coef(4,1) - 0.0D0) > 1e-12) error stop
   if (abs(coef(5,1) - 0.0D0) > 1e-12) error stop

end program

