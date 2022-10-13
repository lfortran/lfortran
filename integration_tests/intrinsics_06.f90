program intrinsics_06
real :: x
integer, parameter :: dp = kind(0.d0)

x = asin(0.84147098)
print *, x

x = acos(0.54030231)
print *, x

x = atan(1.5574077)
print *, x

x = datan(1.5574077_dp)
print *, x

x = asinh(1.1752012)
print *, x

x = acosh(1.5430806)
print *, x

x = atanh(0.76159416)
print *, x

end
