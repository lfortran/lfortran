program intrinsics_325
integer(8) :: d(2) = [1,2]
integer(8) :: spread_(2, 2)
spread_ = spread(d, dim=2, ncopies=2)
print *, spread_
if (spread_(1, 1) /= 1) error stop
if (spread_(1, 2) /= 1) error stop
if (spread_(2, 1) /= 2) error stop
if (spread_(2, 2) /= 2) error stop
end program intrinsics_325
