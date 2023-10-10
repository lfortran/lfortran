program string26
character(len=3) :: s1, s2
character(len=6) :: s
s1 = "abc"
s2 = "def"
s = s1//s2
print *, s
end program
