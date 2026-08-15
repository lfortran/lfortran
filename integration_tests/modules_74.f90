module modules_74_mod
   implicit none
contains
   pure function copy(string) result(aline)
      character(len=*), intent(in)  :: string
      character(len=:), allocatable :: aline
      aline = string
   end function copy
end module modules_74_mod

program modules_74
   use modules_74_mod, only : copy
   use modules_74_mod, only : cp => copy
   implicit none
   character(len=:), allocatable :: astr
   astr = copy('this is a string')
   if (astr /= cp('this is a string')) error stop
   print *, astr
end program modules_74
