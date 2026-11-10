program intrinsics_513
implicit none
! The kind with the smallest decimal precision that satisfies the request.
! real(8) has precision 15, x87 real(10) has precision 18, real(16) has 33.
! p = 16 and p = 18 therefore select kind 10, not kind 16.
integer, parameter :: k15 = selected_real_kind(15)
integer, parameter :: k16 = selected_real_kind(16)
integer, parameter :: k18 = selected_real_kind(18)
integer, parameter :: k19 = selected_real_kind(19)
integer, parameter :: k33 = selected_real_kind(33)
integer, parameter :: k_range = selected_real_kind(16, 4000)
integer :: p

if (k15 /= 8) error stop
if (k16 /= 10) error stop
if (k18 /= 10) error stop
if (k19 /= 16) error stop
if (k33 /= 16) error stop
if (k_range /= 10) error stop

p = 16
if (selected_real_kind(p) /= 10) error stop
p = 18
if (selected_real_kind(p) /= 10) error stop
p = 15
if (selected_real_kind(p) /= 8) error stop
p = 19
if (selected_real_kind(p) /= 16) error stop
end program
