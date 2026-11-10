program intrinsics_513
use iso_c_binding, only: c_long_double
implicit none
! The kind with the smallest decimal precision that satisfies the request.
! real(8) has precision 15. Where x87 real(10) exists (c_long_double == 10,
! precision 18, range 4931), p = 16 and p = 18 select kind 10. Otherwise
! they select kind 16. p = 19 and p = 33 are past kind 10, so both select 16.
! A kind parameter, not a literal 10, so the source still compiles where
! real(10) is absent (Apple arm64 and MSVC, where c_long_double is 8).
integer, parameter :: k_ext = merge(10, 16, c_long_double == 10)
integer, parameter :: k15 = selected_real_kind(15)
integer, parameter :: k16 = selected_real_kind(16)
integer, parameter :: k18 = selected_real_kind(18)
integer, parameter :: k19 = selected_real_kind(19)
integer, parameter :: k33 = selected_real_kind(33)
integer, parameter :: k_range = selected_real_kind(16, 4000)
integer, parameter :: r_ext = range(1.0_k_ext)
real(16), parameter :: widened = real(1.0_k_ext + epsilon(1.0_k_ext), 16)
integer :: p
real(k_ext) :: base, ex, powv

if (k15 /= 8) error stop
if (k16 /= k_ext) error stop
if (k18 /= k_ext) error stop
if (k19 /= 16) error stop
if (k33 /= 16) error stop
if (k_range /= k_ext) error stop

p = 16
if (selected_real_kind(p) /= k_ext) error stop
p = 18
if (selected_real_kind(p) /= k_ext) error stop
p = 15
if (selected_real_kind(p) /= 8) error stop
p = 19
if (selected_real_kind(p) /= 16) error stop

if (c_long_double == 10) then
    if (r_ext /= 4931) error stop
    if (range(1.0_k_ext) /= 4931) error stop
    ! 1 + eps(real(10)) is 1 + 2^-63. real(..., 16) must keep that bit.
    if (widened == 1.0_16) error stop
    base = 2.0_k_ext
    ex = 1.5_k_ext
    powv = base ** ex
    if (powv /= powv) error stop
    if (abs(powv - 2.8284271247461900976_k_ext) > 1.0e-17_k_ext) error stop
end if
end program
