program read_67
implicit none

character(100) :: input

! integer kinds
integer(1) :: i8_1, i8_2
integer(2) :: i16_1, i16_2
integer(4) :: i32_1, i32_2
integer(8) :: i64_1, i64_2

! real kinds
real(4) :: r32_1, r32_2
real(8) :: r64_1, r64_2

! string
character(10) :: s1, s2

! ================================
! 1. INTEGER EMPTY FIELD
! ================================
input = "10,,20"

i8_1 = -1; i8_2 = -1
read(input, *) i8_1, i8_2
if (i8_1 /= 10) error stop "i8 first wrong"
if (i8_2 /= -1) error stop "i8 empty not preserved"

i16_1 = -1; i16_2 = -1
read(input, *) i16_1, i16_2
if (i16_1 /= 10) error stop "i16 first wrong"
if (i16_2 /= -1) error stop "i16 empty not preserved"

i32_1 = -1; i32_2 = -1
read(input, *) i32_1, i32_2
if (i32_1 /= 10) error stop "i32 first wrong"
if (i32_2 /= -1) error stop "i32 empty not preserved"

i64_1 = -1; i64_2 = -1
read(input, *) i64_1, i64_2
if (i64_1 /= 10) error stop "i64 first wrong"
if (i64_2 /= -1) error stop "i64 empty not preserved"

! ================================
! 2. FLOAT EMPTY FIELD
! ================================
input = "1.5,,2.5"

r32_1 = -1.0; r32_2 = -1.0
read(input, *) r32_1, r32_2
if (abs(r32_1 - 1.5) > 1e-6) error stop "f32 first wrong"
if (r32_2 /= -1.0) error stop "f32 empty not preserved"

r64_1 = -1.0d0; r64_2 = -1.0d0
read(input, *) r64_1, r64_2
if (abs(r64_1 - 1.5d0) > 1d-12) error stop "f64 first wrong"
if (r64_2 /= -1.0d0) error stop "f64 empty not preserved"

! ================================
! 3. STRING EMPTY FIELD
! ================================
input = "hello,,world"

s1 = "INIT"; s2 = "INIT"
read(input, *) s1, s2

if (trim(s1) /= "hello") error stop "string first wrong"
if (trim(s2) /= "INIT") error stop "string empty not preserved"

! ================================
! 4. MULTIPLE EMPTY FIELDS
! ================================
input = "7,,,8"

i32_1 = -1; i32_2 = -1
read(input, *) i32_1, i32_2

if (i32_1 /= 7) error stop "multi first wrong"
if (i32_2 /= -1) error stop "multi empty not preserved"

! ================================
! SUCCESS
! ================================
print *, "All tests passed"

end program read_67