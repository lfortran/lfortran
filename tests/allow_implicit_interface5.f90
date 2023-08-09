subroutine klvnzo_(nt,kd)
   implicit double precision (a-h,o-z)
   dimension zo(nt),rt0(8)
   call klvna(rt)
   return
end

program main
   call klvnzo_(5,8)
end program
