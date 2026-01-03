program intrinsics_314
print *, dshiftl( z"A", 10, 5)
if (dshiftl( z"A", 10, 5) /= 320) error stop
end program
