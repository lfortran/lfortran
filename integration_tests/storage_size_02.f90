! storage_size of characters whose length is known only at run time:
! deferred-length allocatable and pointer scalars and arrays, assumed-length
! and deferred-length dummy arguments, and automatic-length locals.
program storage_size_02
    implicit none
    character(len=:), allocatable :: a, b(:), c(:,:)
    character(len=:), pointer :: p, q(:)
    character(len=7), target :: t, u(3)
    integer :: n(3)
    integer(1) :: k1
    integer(2) :: k2
    integer(8) :: k8

    a = "hello"
    if (storage_size(a) /= 40) error stop 1
    k1 = storage_size(a, 1)
    if (k1 /= 40_1) error stop 4
    a = ""
    if (storage_size(a) /= 0) error stop 2
    a = "abcdefghijklmnopqrstuvwxyz"
    if (storage_size(a) /= 208) error stop 3

    k2 = storage_size(a, 2)
    k8 = storage_size(a, 8)
    if (k2 /= 208_2) error stop 5
    if (k8 /= 208_8) error stop 6
    if (kind(storage_size(a, 8)) /= 8) error stop 7
    if (kind(storage_size(a, 2)) /= 2) error stop 8

    allocate(character(len=11) :: b(4))
    if (storage_size(b) /= 88) error stop 9
    if (storage_size(b(2)) /= 88) error stop 10
    if (storage_size(b, 8) /= 88_8) error stop 11
    deallocate(b)
    allocate(character(len=3) :: b(0))
    if (storage_size(b) /= 24) error stop 12

    allocate(character(len=2) :: c(2, 5))
    if (storage_size(c) /= 16) error stop 13

    t = "pointer"
    p => t
    if (storage_size(p) /= 56) error stop 14
    allocate(character(len=13) :: p)
    if (storage_size(p) /= 104) error stop 15
    deallocate(p)

    q => u
    if (storage_size(q) /= 56) error stop 16
    if (storage_size(q(3), 8) /= 56_8) error stop 17

    n = 1
    n = n * storage_size(a)
    if (any(n /= 208)) error stop 18

    call assumed_length("xyz", 24)
    call assumed_length("", 0)
    call assumed_length(a, 208)
    call assumed_length_array(u, 56)
    call deferred_length(a, 208)
    call deferred_length_array(b, 24)
    call pointer_dummy(q, 56)
    call automatic_length(9)
    print *, storage_size(a), storage_size(b), storage_size(c), storage_size(q)

contains

    subroutine assumed_length(s, bits)
        character(len=*), intent(in) :: s
        integer, intent(in) :: bits
        if (storage_size(s) /= bits) error stop 20
        if (storage_size(s, 8) /= int(bits, 8)) error stop 21
    end subroutine

    subroutine assumed_length_array(s, bits)
        character(len=*), intent(in) :: s(:)
        integer, intent(in) :: bits
        if (storage_size(s) /= bits) error stop 22
    end subroutine

    subroutine deferred_length(s, bits)
        character(len=:), allocatable, intent(inout) :: s
        integer, intent(in) :: bits
        if (storage_size(s) /= bits) error stop 23
        s = "four"
        if (storage_size(s) /= 32) error stop 24
        s = "abcdefghijklmnopqrstuvwxyz"
    end subroutine

    subroutine deferred_length_array(s, bits)
        character(len=:), allocatable, intent(in) :: s(:)
        integer, intent(in) :: bits
        if (storage_size(s) /= bits) error stop 25
    end subroutine

    subroutine pointer_dummy(s, bits)
        character(len=:), pointer, intent(in) :: s(:)
        integer, intent(in) :: bits
        if (storage_size(s) /= bits) error stop 26
    end subroutine

    subroutine automatic_length(m)
        integer, intent(in) :: m
        character(len=m) :: s
        character(len=2*m) :: v(2)
        if (storage_size(s) /= 8*m) error stop 27
        if (storage_size(v) /= 16*m) error stop 28
    end subroutine

end program
