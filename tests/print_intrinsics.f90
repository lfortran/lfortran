program print_intrinsics
   implicit none
   integer,parameter:: sp = kind(1e0), dp = kind(1d0)
   print *, 'pi=acos(-1.0_sp)  ="', acos(-1.0_sp),'"'
   print *, 'pi=acos(-1.0_dp)  ="', acos(-1.0_dp),'"'
end program

