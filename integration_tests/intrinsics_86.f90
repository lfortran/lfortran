program intrinsics_86
real(4) :: x
integer :: res
res = kind(3.0)
print *, res
if (res /= 4) error stop
res = kind(x)
print *, res
if (res /= 4) error stop
res = kind(3)
print *, res
if (res /= 4) error stop
res = kind(3.0d0)
print *, res
if (res /= 8) error stop
res = kind(3d0)
print *, res
if (res /= 8) error stop
res = kind(3.0e0)
print *, res
if (res /= 4) error stop
res = kind("a")
print *, res
if (res /= 1) error stop
end program
