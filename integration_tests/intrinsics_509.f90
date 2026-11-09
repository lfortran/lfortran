program intrinsics_509
implicit none
! selected_real_kind must return a kind whose precision is at least p.
! kind 8 has precision 15. kind 16 (binary128) has precision 33.
! On x86, GFortran also has kind 10 (precision 18), so p=16 selects 10
! there and 16 here. p=30 and p=33 are past kind 10, so both select 16.
integer, parameter :: dp = selected_real_kind(15)
integer, parameter :: k16 = selected_real_kind(16)
integer, parameter :: k30 = selected_real_kind(30)
integer, parameter :: k33 = selected_real_kind(33)
real(k16) :: x16
real(k30) :: x30
real(k33) :: x33
integer :: p

if (dp /= 8) error stop
if (k30 /= 16) error stop
if (k33 /= 16) error stop
if (precision(x30) /= 33 .or. range(x30) /= 4931) error stop
if (precision(x33) /= 33 .or. range(x33) /= 4931) error stop
! x86 GFortran selects kind 10 (80-bit, precision 18) for p=16.
if (k16 == 16) then
    if (precision(x16) /= 33) error stop
else if (k16 == 10) then
    if (precision(x16) < 16) error stop
else
    error stop
end if
if (kind(x16) /= k16) error stop
if (kind(x30) /= 16) error stop
if (kind(x33) /= 16) error stop

p = 15
if (selected_real_kind(p) /= 8) error stop
p = 16
if (selected_real_kind(p) /= k16) error stop
p = 30
if (selected_real_kind(p) /= 16) error stop
p = 33
if (selected_real_kind(p) /= 16) error stop
end program
