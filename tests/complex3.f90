program complex3
complex*16 :: x
complex(8) :: y
complex*8 :: z
complex(4) :: w
x = (3.0_8, 4.0_8)
print *, x
y = (3.0_8, 4.0_8)
print *, y
z = (3.0_4, 4.0_4)
print *, z
w = (3.0_4, 4.0_4)
print *, w
end program
