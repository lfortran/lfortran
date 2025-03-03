module strings_48_mod
   implicit none
   character(16) :: option='potential'
end module

program strings_48
   use strings_48_mod
   implicit none
   character(60) :: value
   value = "val1"
   read(value, *) option
   if (option /= "val1") error stop
   option = "val2"
   if (option /= "val2") error stop
end program strings_48