program intrinsics_495
    ! cshift and eoshift of deferred-length character arrays (#14451)
    implicit none
    character(:), allocatable :: s(:), r(:)

    allocate(character(2) :: s(4))
    s = ["bb", "cc", "aa", "dd"]

    call check(cshift(s(1:2), 1), ["cc", "bb"])
    call check(cshift(s(1:3), -1), ["aa", "bb", "cc"])
    call check(cshift(s, 1), ["cc", "aa", "dd", "bb"])
    call check(eoshift(s(1:2), 1, "zz"), ["cc", "zz"])
    call check(eoshift(s(1:3), -2, "zz"), ["zz", "zz", "bb"])

    allocate(character(2) :: r(2))
    r = cshift(s(1:2), 1)
    call check(r, ["cc", "bb"])
    r = eoshift(s(1:2), -1, "zz")
    call check(r, ["zz", "bb"])

contains

    subroutine check(res, expected)
        character(*), intent(in) :: res(:), expected(:)
        integer :: i
        if (len(res) /= 2) error stop 1
        if (size(res) /= size(expected)) error stop 2
        do i = 1, size(expected)
            if (res(i) /= expected(i)) error stop 3
        end do
    end subroutine

end program
