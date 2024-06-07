program intrinsics_103
implicit none

character(len=*), parameter :: magic_number = char(60 + 5)
! character(len=*), parameter :: magic_number2 = char(int(z"41"))   TODO: Support Boz constant

print *, magic_number
! print *, magic_number2

if (magic_number /= "A") error stop
! if (magic_number2 /= "A") error stop

end program
