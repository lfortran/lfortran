program string_39
implicit none
character(len=8) :: s
s = "lfortran"
print*, "s:", s(+(1+1):4)
if (s(+(1+1):4) /= "for") error stop
end program
