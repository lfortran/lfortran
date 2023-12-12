program intrinsics_90
integer :: a(3)
a = [1, 2, 3]
print *, shape(a)
print *, size(a)
print *, maxval(a)
print *, minval(a)
print *, sum(a)
end program
