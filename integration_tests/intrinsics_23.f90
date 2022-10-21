program intrinsics_23
implicit none
integer(kind=4), parameter :: i32 = huge(0)
integer(kind=8), parameter :: i64 = huge(0_8)
real(kind=4), parameter :: y = huge(0.0)
real(kind=8), parameter :: z = huge(0.0d0)

print *, i32, i64, y, z
print *, huge(0), huge(0_8), huge(0.0), huge(0.0d0)

end program
