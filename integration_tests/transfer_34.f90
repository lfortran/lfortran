program transfer_34
    use iso_c_binding, only: c_char, c_int
    implicit none

    type, bind(c) :: cattr_t
        character(kind=c_char, len=1) :: name(4)
        integer(c_int) :: value
    end type cattr_t

    type :: sattr_t
        sequence
        integer :: id
        character(len=1) :: name(4)
        character(len=6) :: label
        integer :: value
    end type sattr_t

    type(cattr_t) :: c
    type(cattr_t) :: carr(3)
    type(cattr_t), allocatable :: calloc(:)
    type(sattr_t) :: s
    type(sattr_t) :: sarr(2)
    character(len=4) :: str4
    character(len=6) :: str6
    character(len=2) :: pair(2)
    character(len=1) :: chars(6)
    integer :: i

    ! bind(C): scalar struct, character mold
    c%name = ['a', 'b', 'c', 'd']
    c%value = 42
    str4 = transfer(c%name, str4)
    print *, str4
    if (str4 /= 'abcd') error stop
    if (c%value /= 42) error stop

    ! bind(C): array mold
    pair = transfer(c%name, pair)
    print *, pair
    if (pair(1) /= 'ab') error stop
    if (pair(2) /= 'cd') error stop

    ! bind(C): element of a fixed-size array of structs, runtime index
    do i = 1, 3
        carr(i)%name = [achar(iachar('e') + i), 'f', 'g', 'h']
        carr(i)%value = i
    end do
    do i = 1, 3
        str4 = transfer(carr(i)%name, str4)
        print *, i, str4
        if (str4 /= achar(iachar('e') + i) // 'fgh') error stop
        if (carr(i)%value /= i) error stop
    end do

    ! bind(C): element of an allocatable array of structs, in a loop
    ! (the pattern that crashed in mglet's fieldio2_mod)
    allocate(calloc(2))
    calloc(1)%name = ['M', 'I', 'N', 'L']
    calloc(2)%name = ['M', 'A', 'X', 'L']
    do i = 1, size(calloc)
        str4 = transfer(calloc(i)%name, str4)
        print *, str4
        if (i == 1 .and. str4 /= 'MINL') error stop
        if (i == 2 .and. str4 /= 'MAXL') error stop
    end do
    deallocate(calloc)

    ! SEQUENCE: array member, character mold
    s%id = 7
    s%name = ['w', 'x', 'y', 'z']
    s%label = 'labels'
    s%value = -3
    str4 = transfer(s%name, str4)
    print *, str4
    if (str4 /= 'wxyz') error stop

    ! SEQUENCE: scalar member with len > 1, character and array molds
    str6 = transfer(s%label, str6)
    print *, str6
    if (str6 /= 'labels') error stop
    chars = transfer(s%label, chars)
    print *, chars
    if (chars(1) /= 'l' .or. chars(6) /= 's') error stop

    ! Members around the character members are untouched
    if (s%id /= 7) error stop
    if (s%value /= -3) error stop

    ! SEQUENCE: element of an array of structs
    sarr(1)%name = ['1', '2', '3', '4']
    sarr(2)%name = ['5', '6', '7', '8']
    do i = 1, 2
        str4 = transfer(sarr(i)%name, str4)
        print *, str4
        if (i == 1 .and. str4 /= '1234') error stop
        if (i == 2 .and. str4 /= '5678') error stop
    end do
end program transfer_34
