double precision function d1mach()
integer small(2)
double precision dmach(5)
equivalence (dmach(1),small(1))
d1mach = 1.3d0
end function

program equivalence_08
   interface
      double precision function d1mach()
      end function
   end interface
   print *, d1mach()
   if (abs(d1mach() - 1.3d0) > 1e-10) error stop
end program
