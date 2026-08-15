module modules_73_mod
   implicit none
contains
   pure function copy(string) result(aline)
      character(len=*), intent(in)  :: string
      character(len=:), allocatable :: aline
      aline = string
   end function copy
end module modules_73_mod

program modules_73
   use modules_73_mod, only : copy
   use modules_73_mod, only : cp => copy
   implicit none
   character(len=:), allocatable :: astr
   astr = copy('this is a string')
   if (astr /= cp('this is a string')) error stop
   print *, astr
end program modules_73
