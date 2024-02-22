program intrinsics_149
integer :: vector(2)  = [1, 1]
logical :: mask(2,2)
integer :: field(2,2), unity(2,2)

real :: vector_r(6)
logical :: mask_r(3,2)
real :: field_r(3,2), unity_r(3,2)

double precision :: vector_d(6)
logical :: mask_d(6)
double precision :: field_d(6), unity_d(6)

real :: vector_(4)
logical :: mask_(4)
real :: field_(4), unity_(4)

real :: unity__(4)

complex :: vector_c(8)
logical :: mask_c(2, 4)
complex :: field_c(2, 4), unity_c(2, 4)

! ! result: unity matrix
field = 0
mask = reshape([.true., .false., .false., .true.], shape(mask))
unity = unpack(vector, mask, field)
print *, unity
if (sum(unity) /= 2) error stop
if (unity(1,1) /= 1) error stop
if (unity(2,2) /= 1) error stop

vector_r = 23.12
field_r = 681.31
mask_r = reshape([.true., .false., .false., .true., .true., .false.], shape(mask_r))
unity_r = unpack(vector_r, mask_r, field_r)
print *, unity_r
print *, sum(unity_r)
if (abs(sum(unity_r) - 2113.29004) > 1e-8) error stop

vector_d = 23.12D0
mask_d = .true.
field_r = 681.31D0
unity_d = unpack(vector_d, mask_d, field_d)
print *, unity_d
print *, sum(unity_d)
if (abs(sum(unity_d) - 138.72D0) > 1e-12) error stop

print *, unpack([23.12, 23.12, 23.12, 23.12], [.true., .false., .true., .false.], [1.0, 2.0, 3.0, 4.0])
unity__ = unpack([23.12, 23.12, 23.12, 23.12], [.true., .false., .true., .false.], [1.0, 2.0, 3.0, 4.0])
print *, unity__
print *, sum(unity__)
if (abs(sum(unity__) - 52.2400017) > 1e-8) error stop

vector_ = [23.12, 23.12, 23.12, 23.12]
mask_ = [.true., .false., .true., .false.]
field_ = [1.0, 2.0, 3.0, 4.0]
unity_ = unpack(vector_, mask_, field_)
print *, unity_
print *, sum(unity_)
if (abs(sum(unity_) - 52.2400017) > 1e-8) error stop

vector_c = (23.12, -23.12)
mask_c = reshape([.true., .false., .true., .false., .true., .false., .true., .false.], shape(mask_c))
field_c = (1.10, -91.24)
unity_c = unpack(vector_c, mask_c, field_c)
print *, unity_c
print *, abs(sum(unity_c))
if (abs(abs(sum(unity_c)) - 467.586426) > 1e-8) error stop

end program
