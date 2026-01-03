subroutine sub()
   external f
   double precision f, y
   y = f(1.0d0)
   if (abs(y-1.0d0) >= 1e-8) error stop
   print *, y
end subroutine

double precision function f(x)
   double precision x
   f = x
end function

program main
   call sub()
end program

