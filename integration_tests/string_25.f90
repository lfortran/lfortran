program string_25
character(len=3) :: s1, s2
character(len=4) :: s3
character(len=2) :: s4
s1 = "abc"
s2 = "abc"
s3 = "defe"
s4 = "ab"
if (s1 == s2) then
    print *, "equal"
else
    print *, "not equal"
end if

if (s4 < s1) then
    print *, "less than"
else
    print *, "not less than"
end if

if (s3 > s4) then
    print *, "greater than"
else
    print *, "not greater than"
end if

if (s2 /= s4) then
    print *, "not equal"
else
    print *, "not, not equal"
end if

if (s4 <= s3) then
    print *, "less than equal to"
else
    print *, "not less than equal to"
end if

if (s3 >= s2) then
    print *, "greater than equal to"
else
    print *, "not greater than equal to"
end if
end program
