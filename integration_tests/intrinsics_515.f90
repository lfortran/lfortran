program intrinsics_515
implicit none
! A failed selected_real_kind search is not always -1 (#15147).
! -1: precision unavailable, range available or omitted
! -2: range unavailable, precision available or omitted
! -3: both unavailable
! 10000 and 100000 are past every real kind, so the codes do not
! depend on whether x87 kind 10 exists.
integer, parameter :: k4 = selected_real_kind(6)
integer, parameter :: k8 = selected_real_kind(15)
integer, parameter :: c_prec = selected_real_kind(p=10000)
integer, parameter :: c_range = selected_real_kind(r=100000)
integer, parameter :: c_range_both = selected_real_kind(6, 100000)
integer, parameter :: c_prec_both = selected_real_kind(10000, 37)
integer, parameter :: c_neither = selected_real_kind(10000, 100000)
integer, parameter :: c_radix = selected_real_kind(6, 37, 10)
integer :: p, r, rad

if (k4 /= 4) error stop
if (k8 /= 8) error stop
if (c_prec /= -1) error stop
if (c_range /= -2) error stop
if (c_range_both /= -2) error stop
if (c_prec_both /= -1) error stop
if (c_neither /= -3) error stop
if (c_radix /= -5) error stop

p = 6
if (selected_real_kind(p) /= 4) error stop
p = 15
if (selected_real_kind(p) /= 8) error stop
p = 10000
if (selected_real_kind(p) /= -1) error stop
r = 100000
if (selected_real_kind(r=r) /= -2) error stop
p = 6
r = 100000
if (selected_real_kind(p, r) /= -2) error stop
p = 10000
r = 37
if (selected_real_kind(p, r) /= -1) error stop
p = 10000
r = 100000
if (selected_real_kind(p, r) /= -3) error stop
rad = 10
if (selected_real_kind(6, 37, rad) /= -5) error stop
end program
