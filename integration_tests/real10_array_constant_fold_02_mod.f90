module real10_array_constant_fold_02_mod
    implicit none
    integer, parameter :: xp = selected_real_kind(18)
    integer :: i
    real(xp), parameter :: c(3) = [1.0_xp, 2.0_xp, 4.0_xp] / 3
    real(xp), parameter :: s(4) = sin([(real(i, xp), i = 1, 4)])
    real(xp), parameter :: z(2) = 0
end module
