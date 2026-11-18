! `transfer` of a character array that has no string descriptor to an array
! mold of another element size: a C character buffer (a dummy argument of a
! `bind(C)` procedure) and a component stored inside a `bind(C)` or `sequence`
! derived type.
module transfer_41_mod
    use iso_c_binding, only: c_char, c_int
    implicit none
    type, bind(C) :: record
        integer(c_int) :: id
        character(kind=c_char) :: tag(4)
        character(kind=c_char) :: name(8)
    end type
    type :: packed
        sequence
        integer :: id
        character(4) :: words(2)
    end type
    integer(1), parameter :: bytes(8) = [integer(1) :: 119, 120, 121, 122, &
        112, 113, 114, 115]
contains
    subroutine c_buffer(s, r) bind(C)
        character(kind=c_char), intent(in) :: s(8)
        type(record), intent(in) :: r
        integer(1) :: b(8)
        integer(2) :: k2(4)
        integer(4) :: k4(2), tag(1)
        integer(8) :: k8(1)
        b = transfer(s, b)
        if (any(b /= bytes)) error stop 1
        k2 = transfer(s, k2)
        if (any(k2 /= transfer(bytes, k2))) error stop 2
        k4 = transfer(s, k4)
        if (any(k4 /= transfer(bytes, k4))) error stop 3
        k8 = transfer(s, k8)
        if (any(k8 /= transfer(bytes, k8))) error stop 4
        if (any(transfer(s, [0_8]) /= transfer(bytes, k8))) error stop 5
        if (size(transfer(s, [0_2])) /= 4) error stop 6
        k4 = transfer(r%name, k4)
        if (any(k4 /= transfer(bytes, k4))) error stop 7
        tag = transfer(r%tag, tag)
        if (any(tag /= transfer(bytes(5:8), tag))) error stop 8
    end subroutine

    subroutine c_buffer_n(s, n) bind(C)
        integer(c_int), value :: n
        character(kind=c_char), intent(in) :: s(n)
        integer(4) :: k4(2)
        k4 = transfer(s, k4)
        if (any(k4 /= transfer(bytes, k4))) error stop 9
        if (any(transfer(s, [0_8]) /= transfer(bytes, [0_8]))) error stop 10
    end subroutine
end module

program transfer_41
    use transfer_41_mod
    implicit none
    type(record) :: r
    type(packed) :: p
    character(kind=c_char) :: buf(8)
    integer(1) :: b(8)
    integer(4) :: k4(2), tag(1)
    integer(8) :: k8(1)

    r%id = 1
    r%tag = ["p", "q", "r", "s"]
    r%name = ["w", "x", "y", "z", "p", "q", "r", "s"]
    b = transfer(r%name, b)
    if (any(b /= bytes)) error stop 11
    k4 = transfer(r%name, k4)
    if (any(k4 /= transfer(bytes, k4))) error stop 12
    k8 = transfer(r%name, k8)
    if (any(k8 /= transfer(bytes, k8))) error stop 13
    tag = transfer(r%tag, tag)
    if (any(tag /= transfer(bytes(5:8), tag))) error stop 14
    if (size(transfer(r%name, [0_2])) /= 4) error stop 15

    p%id = 2
    p%words = ["wxyz", "pqrs"]
    b = transfer(p%words, b)
    if (any(b /= bytes)) error stop 16
    k4 = transfer(p%words, k4)
    if (any(k4 /= transfer(bytes, k4))) error stop 17

    buf = r%name
    call c_buffer(buf, r)
    call c_buffer_n(buf, 8_c_int)
    print *, "OK"
end program transfer_41
