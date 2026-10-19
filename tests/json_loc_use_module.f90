program json_loc_use_module
use json_loc_module_m, only: ik, s
implicit none
integer :: a(2)
a = ik
call s(a)
end program
