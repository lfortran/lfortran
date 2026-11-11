program intrinsics_514
use iso_c_binding, only: c_long_double
implicit none
! sqrt of real(10). That kind exists only where the C long double kind is 10
! (Linux x86-64). A kind parameter, not a literal 10, so this still compiles
! where real(10) is absent (Apple arm64 and MSVC, where c_long_double is 8
! and selected_real_kind(16) is 16).
integer, parameter :: k10 = merge(10, 16, c_long_double == 10)
real(k10) :: x, s
real(16) :: q
if (c_long_double == 10) then
    ! Compile-time sqrt(2) was folded from the kind-10 pointer payload read
    ! as a double, which printed as 0.
    if (abs(sqrt(2.0_k10) - 1.4142135623730950488_k10) > 1.0e-17_k10) error stop
    if (abs(sqrt(2.0_k10)**2 - 2.0_k10) > 1.0e-17_k10) error stop
    x = 2.0_k10
    s = sqrt(x)
    if (abs(s - 1.4142135623730950488_k10) > 1.0e-17_k10) error stop
    if (abs(s*s - 2.0_k10) > 1.0e-17_k10) error stop
end if
! Kind 16 already squares back to 2 and must stay that way.
q = 2.0_16
if (abs(sqrt(q)**2 - 2.0_16) > 1.0e-30_16) error stop
end program
