program bindc_64
    ! c_loc of character members of a bind(c) type
    use iso_c_binding, only: c_char, c_int, c_intptr_t, c_ptr, c_loc, &
        c_associated, c_f_pointer
    implicit none
    type, bind(c) :: t
        integer(c_int) :: i
        character(kind=c_char) :: s
        character(kind=c_char) :: a(4)
    end type
    type(t), target :: w
    type(c_ptr) :: p
    character(kind=c_char), pointer :: c1
    character(kind=c_char), pointer :: ca(:)
    integer(c_intptr_t) :: base

    w%i = 7
    w%s = 'Z'
    w%a = ['a', 'b', 'c', 'd']
    base = transfer(c_loc(w), base)

    p = c_loc(w%s)
    if (.not. c_associated(p)) error stop
    if (transfer(p, base) - base /= 4) error stop
    call c_f_pointer(p, c1)
    if (c1 /= 'Z') error stop
    c1 = 'Y'
    if (w%s /= 'Y') error stop

    p = c_loc(w%a)
    if (transfer(p, base) - base /= 5) error stop
    call c_f_pointer(p, ca, [4])
    if (ca(3) /= 'c') error stop

    p = c_loc(w%a(2))
    if (transfer(p, base) - base /= 6) error stop
end program
