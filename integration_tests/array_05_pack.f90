program array_05_pack
    implicit none
    character(:), allocatable :: words(:)
    integer :: n

    call check_pack([character(2) :: 'a', 'bb'])
    call check_pack([character(7) :: 'a', 'bb'])
    n = 5
    allocate(character(n) :: words(2))
    words(1) = 'a'
    words(2) = 'bb'
    call check_pack(words)
    deallocate(words)
    allocate(character(n) :: words(0))
    call check_empty(words, n)

contains
    subroutine check_pack(a)
        character(*), intent(in) :: a(:)
        call check_result(pack(a, [.true., .false.]), len(a), 1, a(1:1))
        call check_result(pack(a, [.false., .true.]), len(a), 1, a(2:2))
        call check_result(pack(a, [.false., .false.]), len(a), 0, a(1:0))
        call check_result(pack(a, .true.), len(a), 2, a)
        call check_result(pack(a, .false.), len(a), 0, a(1:0))
        call check_result(pack(a, [.true., .false.], a(2:1:-1)), len(a), 2, [a(1), a(1)])
    end subroutine

    subroutine check_empty(a, length)
        character(*), intent(in) :: a(:)
        integer, intent(in) :: length
        call check_result(pack(a, .true.), length, 0, a)
        call check_result(pack(a, .false.), length, 0, a)
    end subroutine

    subroutine check_result(result, length, count, expected)
        character(*), intent(in) :: result(:), expected(:)
        integer, intent(in) :: length, count
        integer :: i
        if (len(result) /= length) error stop 1
        if (size(result) /= count) error stop 2
        do i = 1, count
            if (result(i) /= expected(i)) error stop 3
            if (len_trim(result(i)) /= len_trim(expected(i))) error stop 4
            if (result(i)(length:length) /= expected(i)(length:length)) error stop 5
        end do
    end subroutine
end program
