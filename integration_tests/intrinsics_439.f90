program intrinsics_439
character(len=1) :: mn, mx

mn = minval(['x','y'])
mx = maxval(['x','y'])

if (mn /= 'x') error stop "minval incorrect"
if (mx /= 'y') error stop "maxval incorrect"

print *, mn
print *, mx

end program intrinsics_439
