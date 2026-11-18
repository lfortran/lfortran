! `transfer` of a character array whose length is not a constant to an
! array mold: the result size is computed from the bytes of the source
! (#17268).
program transfer_40
    implicit none
    character(4) :: labels(2), table(2, 2)
    character(3) :: odd(3)
    character(kind=4, len=2) :: wide(2)
    character(0) :: empty(3)
    integer(1), parameter :: bytes(8) = [integer(1) :: 119, 120, 121, 122, &
        112, 113, 114, 115]

    labels = ["wxyz", "pqrs"]
    call explicit_len(labels, 4)
    call assumed_len(labels)
    call assumed_len_shape(labels)
    call explicit_shape(labels, 2)

    table = reshape(["wxyz", "pqrs", "pqrs", "wxyz"], [2, 2])
    call rank_two(table)

    odd = ["abc", "def", "ghi"]
    call not_a_multiple(odd)

    wide = [4_"wx", 4_"yz"]
    call wide_characters(wide)

    call zero_length(empty, empty)
    print *, "OK"
contains
    subroutine explicit_len(s, n)
        integer, intent(in) :: n
        character(n), intent(in) :: s(2)
        integer(4) :: k(2)
        integer(1) :: b(8)
        k = transfer(s, k)
        if (any(k /= transfer(bytes, k))) error stop 1
        b = transfer(s, b)
        if (any(b /= bytes)) error stop 2
        if (size(transfer(s, [0])) /= 2) error stop 3
    end subroutine

    subroutine assumed_len(s)
        character(*), intent(in) :: s(2)
        integer(4) :: k(2)
        if (size(transfer(s, [0])) /= 2) error stop 4
        if (size(transfer(s, [0_8])) /= 1) error stop 5
        if (size(transfer(s, [0_1])) /= 8) error stop 6
        k = transfer(bytes, k)
        if (any(k /= transfer(s, k))) error stop 7
        if (any(transfer(s, bytes) /= bytes)) error stop 8
    end subroutine

    subroutine assumed_len_shape(s)
        character(*), intent(in) :: s(:)
        integer(4) :: k(2)
        integer(1) :: b(8)
        k = transfer(s, k)
        if (any(k /= transfer(bytes, k))) error stop 9
        b = transfer(s, b)
        if (any(b /= bytes)) error stop 10
        if (size(transfer(s, [0])) /= 2) error stop 11
    end subroutine

    subroutine explicit_shape(s, n)
        integer, intent(in) :: n
        character(*), intent(in) :: s(n)
        integer(1) :: b(8)
        b = transfer(s, b)
        if (any(b /= bytes)) error stop 12
        if (size(transfer(s, [0_2])) /= 4) error stop 13
    end subroutine

    subroutine rank_two(s)
        character(*), intent(in) :: s(:, :)
        integer(1) :: b(16)
        b = transfer(s, b)
        if (any(b(1:8) /= bytes)) error stop 14
        if (any(b(9:12) /= bytes(5:8))) error stop 15
        if (any(b(13:16) /= bytes(1:4))) error stop 16
        if (size(transfer(s, [0])) /= 4) error stop 17
    end subroutine

    subroutine not_a_multiple(s)
        character(*), intent(in) :: s(3)
        if (size(transfer(s, [0])) /= 3) error stop 18
        if (size(transfer(s, [0_2])) /= 5) error stop 19
        if (size(transfer(s, [0_8])) /= 2) error stop 20
        if (any(transfer(s, [0_1]) /= [integer(1) :: 97, 98, 99, 100, 101, &
            102, 103, 104, 105])) error stop 21
    end subroutine

    subroutine wide_characters(s)
        character(kind=4, len=*), intent(in) :: s(2)
        integer(4) :: k(4)
        k = transfer(s, k)
        if (any(k /= [119, 120, 121, 122])) error stop 22
        if (size(transfer(s, [0_1])) /= 16) error stop 23
    end subroutine

    subroutine zero_length(s, z)
        character(*), intent(in) :: s(:)
        character(0), intent(in) :: z(3)
        integer(1) :: b(0)
        if (size(transfer(s, [0_1])) /= 0) error stop 24
        if (size(transfer(s, [0])) /= 0) error stop 25
        if (size(transfer(z, [0_1])) /= 0) error stop 26
        b = transfer(s, b)
        if (size(b) /= 0) error stop 27
        b = transfer(z, b)
        if (size(b) /= 0) error stop 28
    end subroutine
end program transfer_40
