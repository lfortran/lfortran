program array_concat
integer :: A = 2
integer :: B(3) = [1,2,3]
integer :: C(4) 
C = [B, A]
print *, C
if( C(1) /= 1 .or. C(2) /= 2 .or. C(3) /= 3 .or. C(4) /= 2 ) error stop
end program