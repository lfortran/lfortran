program array_06_pack
    implicit none
    character(:), allocatable :: s(:), t(:,:), r(:)
    character(:), pointer :: p(:)
    character(3), target :: buffer(3)
    integer :: n

    allocate(character(2) :: s(3))
    s = ["bb", "cc", "aa"]
    print *, pack(s, [.true., .false., .true.])
    call check(pack(s, [.true., .false., .true.]), 2, ["bb", "aa"])
    call check(pack(s, s /= "cc"), 2, ["bb", "aa"])
    call check(pack(s, .true.), 2, s)
    call check(pack(s, .false.), 2, s(1:0))

    allocate(character(2) :: r(2))
    r = pack(s, [.false., .true., .true.])
    if (len(r) /= 2 .or. size(r) /= 2) error stop 1
    if (r(1) /= "cc" .or. r(2) /= "aa") error stop 2

    allocate(character(2) :: t(2, 2))
    t = reshape(["ab", "cd", "ef", "gh"], [2, 2])
    call check(pack(t, t /= "cd"), 2, ["ab", "ef", "gh"])

    n = 5
    deallocate(s)
    allocate(character(n) :: s(2))
    s(1) = "a"
    s(2) = "bbbbb"
    call check(pack(s, [.false., .true.]), n, ["bbbbb"])
    call check_allocatable_dummy(s)

    buffer = ["xyz", "uvw", "rst"]
    p => buffer
    call check(pack(p, [.true., .true., .false.]), 3, ["xyz", "uvw"])

contains

    subroutine check_allocatable_dummy(a)
        character(:), allocatable, intent(in) :: a(:)
        call check(pack(a, [.true., .false.]), len(a), a(1:1))
    end subroutine

    subroutine check(res, length, expected)
        character(*), intent(in) :: res(:), expected(:)
        integer, intent(in) :: length
        integer :: i
        if (len(res) /= length) error stop 3
        if (size(res) /= size(expected)) error stop 4
        do i = 1, size(expected)
            if (res(i) /= expected(i)) error stop 5
        end do
    end subroutine

end program
