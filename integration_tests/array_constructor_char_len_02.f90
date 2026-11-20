program array_constructor_char_len_02
    ! A type-spec whose length is known only at runtime pads or cuts
    ! constant elements
    implicit none
    integer :: n
    character(len=10) :: buf
    character(len=8) :: cut(3)
    character(len=:), allocatable :: s(:)

    n = 4
    write(buf, '(*(a))') [character(len=n) :: 'a', 'bb']
    if (buf /= 'a   bb    ') error stop 1

    cut = [character(len=n) :: 'a', 'bb', 'abcdefgh']
    if (cut(1) /= 'a' .or. cut(2) /= 'bb' .or. cut(3) /= 'abcd') error stop 2

    allocate(character(len=n) :: s(3))
    s = [character(len=n) :: 'one', 'three', 'seven']
    if (len(s) /= 4) error stop 3
    if (s(1) /= 'one ' .or. s(2) /= 'thre' .or. s(3) /= 'seve') error stop 4

    if (len([character(len=n) :: 'a', 'bb']) /= 4) error stop 5
    if (size([character(len=n) :: 'a', 'bb', 'ccc']) /= 3) error stop 6

    n = 2
    if (any([character(len=n) :: 'xyz', 'w'] /= ['xy', 'w '])) error stop 7
    print *, "OK"
end program
