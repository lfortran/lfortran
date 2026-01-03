program intrinsics_149
type :: my_type
    integer :: a
end type
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
integer, parameter :: unity_comp(4) = unpack([1, 2, 3, 4], [.true., .false., .true., .false.], [5, 6, 7, 8])
real, parameter :: unity_comp_r(4) = unpack([23.12, 23.12, 23.12, 23.12], [.false., .true., .true., .false.],&
    [681.31, 681.31, 681.31, 681.31])

type(my_type), dimension(4) :: struct_field
type(my_type), dimension(2) :: struct_vec
type(my_type) :: struct_res(4)

print *, unity_comp
if (sum(unity_comp) /= 17) error stop

print*, unity_comp_r
print*, sum(unity_comp_r)
if (abs(sum(unity_comp_r) - 1408.85999) > 1e-5) error stop

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

struct_vec(1)%a = 10
struct_vec(2)%a = 20
struct_field = [ my_type(1), my_type(2), my_type(3), my_type(4) ]
struct_res = unpack(struct_vec, mask_, struct_field)
if (struct_res(1)%a /= 10 .or. struct_res(3)%a /= 20) error stop
if (struct_res(2)%a /= 2 .or. struct_res(4)%a /= 4) error stop

end program
