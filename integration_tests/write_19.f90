program write_19
   use write_19_mod, only: t
   use write_19_io, only: print_it
   implicit none
   type(t) :: obj
   call print_it(obj)
   print *, "ok"
end program
