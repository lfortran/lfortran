program intrinsics_426
    ! Test count(mask, dim) with variable (non-constant) dim
    implicit none
    logical :: mask(3,4)
    integer :: dim, i, j
    integer :: res1(4), res2(3)

    mask = .false.
    do i = 1, 3
        do j = 1, 4
            if (mod(i + j, 2) == 0) mask(i, j) = .true.
        end do
    end do

    ! dim=1: count along first dimension for each column
    dim = 1
    res1 = count(mask, dim)
    if (res1(1) /= 2) error stop
    if (res1(2) /= 1) error stop
    if (res1(3) /= 2) error stop
    if (res1(4) /= 1) error stop

    ! dim=2: count along second dimension for each row
    dim = 2
    res2 = count(mask, dim)
    if (res2(1) /= 2) error stop
    if (res2(2) /= 2) error stop
    if (res2(3) /= 2) error stop

    ! Also test with subroutine call (submodule pattern)
    call test_sub(mask, 1, res1)
    if (res1(1) /= 2) error stop
    if (res1(2) /= 1) error stop
    if (res1(3) /= 2) error stop
    if (res1(4) /= 1) error stop

contains
    subroutine test_sub(m, d, r)
        logical, intent(in) :: m(:,:)
        integer, intent(in) :: d
        integer, intent(out) :: r(:)
        r = count(m, d)
    end subroutine
end program
