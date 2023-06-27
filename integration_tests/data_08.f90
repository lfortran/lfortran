subroutine gamo()
   implicit double precision (A-H,O-Z)
   dimension g(2)
   data g/1.5D0,2.5D0/
   if (abs(g(1)-1.5D0) > 1e-12) error stop
   if (abs(g(2)-2.5D0) > 1e-12) error stop
   print *, g
end subroutine

program main
   call gamo()
end program

