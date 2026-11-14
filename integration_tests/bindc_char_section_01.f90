module bindc_char_section_01_mod
    use iso_c_binding, only: c_char, c_int
    implicit none
    interface
        integer(c_int) function matches(buf, expected, n) bind(c, name='bindc_char_section_01_matches')
            import :: c_char, c_int
            type(*), intent(in) :: buf(*)
            character(kind=c_char), intent(in) :: expected(*)
            integer(c_int), value :: n
        end function

        subroutine overwrite(buf, value, n) bind(c, name='bindc_char_section_01_overwrite')
            import :: c_char, c_int
            type(*), intent(inout) :: buf(*)
            character(kind=c_char), intent(in) :: value(*)
            integer(c_int), value :: n
        end subroutine
    end interface
contains
    subroutine check_forwarded(x)
        character(len=2), intent(in) :: x(:)
        if (matches(x, 'abef', 4) /= 0) error stop 5
    end subroutine

    subroutine overwrite_forwarded(x)
        character(len=2), intent(inout) :: x(:)
        call overwrite(x, 'qrst', 4)
    end subroutine
end module

program bindc_char_section_01
    use bindc_char_section_01_mod
    implicit none
    character(len=2) :: a(4)
    character(len=2) :: s

    a = ['ab', 'cd', 'ef', 'gh']
    s = 'ab'
    if (matches(a(1:4:2), 'abef', 4) /= 0) error stop 1
    if (matches(a(4:1:-2), 'ghcd', 4) /= 0) error stop 2
    if (matches(a(2:3), 'cdef', 4) /= 0) error stop 3
    if (matches(a, 'abcdefgh', 8) /= 0) error stop 4
    call check_forwarded(a(1:4:2))
    if (matches(s, 'ab', 2) /= 0) error stop 6

    call overwrite(a(1:4:2), 'ijkl', 4)
    if (any(a /= ['ij', 'cd', 'kl', 'gh'])) error stop 7
    call overwrite(a(4:1:-2), 'mnop', 4)
    if (any(a /= ['ij', 'op', 'kl', 'mn'])) error stop 8
    call overwrite_forwarded(a(4:1:-2))
    if (any(a /= ['ij', 'st', 'kl', 'qr'])) error stop 9
    call overwrite(a(1:0), 'x', 0)
    if (any(a /= ['ij', 'st', 'kl', 'qr'])) error stop 10
end program
