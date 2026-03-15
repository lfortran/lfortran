program submodule_44
   ! Test: renaming a re-exported generic interface to match
   ! one of its specific submodule procedure names.
   use submodule_44_reexport, only: mytype, create2 => create
   implicit none
   type(mytype) :: t
   t = create2(1, 2)
   if (t%x /= 3) error stop
   print *, "ok"
end program submodule_44
