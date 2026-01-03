program intrinsics_53
integer(4) :: n4 = 3
integer(8) :: n8 = 3
print *, repeat("a", n4)
print *, repeat("a", n8)
if (repeat("a", n4) /= "aaa") error stop
if (repeat("a", n8) /= "aaa") error stop
end program
