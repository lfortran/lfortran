! An entity-decl char-length overrides the length in the type spec
! (F2018 7.4.4.2 paragraph 4).
module character_37_m
    implicit none
    interface
        subroutine character_37_out(c)
            character(len=*), intent(out) :: c*4
        end subroutine
    end interface
contains
    subroutine s_star(c)
        character(*) :: c*4
        if (len(c) /= 4) error stop 1
        if (c /= 'abcd') error stop 2
    end subroutine

    subroutine s_kind(c)
        character(kind=1, len=*) :: c*4
        if (len(c) /= 4) error stop 3
    end subroutine

    subroutine s_paren(c)
        character(len=*), intent(in) :: c*(4)
        if (len(c) /= 4) error stop 4
    end subroutine

    subroutine s_array(c)
        character(len=*) :: c(2)*4
        if (len(c) /= 4) error stop 5
        if (c(2) /= 'efgh') error stop 6
    end subroutine

    subroutine s_second(a, c)
        character(len=*) :: a, c*4
        if (len(a) /= 7) error stop 7
        if (len(c) /= 4) error stop 8
    end subroutine

    integer function s_assumed(c) result(n)
        character(len=4) :: c*(*)
        n = len(c)
    end function
end module

subroutine character_37_out(c)
    character(len=4), intent(out) :: c
    c = 'wxyz'
end subroutine

program character_37
    use character_37_m
    implicit none
    character(len=4) :: arr(2), s
    character(len=:), allocatable :: a*4
    character(len=:), pointer :: q*4
    character(len=*), parameter :: pc*4 = 'abcdefgh'
    character(len=*), parameter :: pd*4 = 'ab'

    arr = ['abcd', 'efgh']
    call s_star('abcd')
    call s_kind('abcd')
    call s_paren('abcd')
    call s_array(arr)
    call s_second('1234567', 'abcd')

    call character_37_out(s)
    if (s /= 'wxyz') error stop 9

    allocate(a, q)
    a = 'abcdefgh'
    q = 'ijklmnop'
    if (len(a) /= 4) error stop 10
    if (a /= 'abcd') error stop 11
    if (len(q) /= 4) error stop 12
    if (q /= 'ijkl') error stop 13
    deallocate(a, q)

    if (s_assumed('abcdefgh') /= 8) error stop 14
    if (s_assumed('xy') /= 2) error stop 15

    if (len(pc) /= 4) error stop 16
    if (len(pd) /= 4) error stop 17
    if (pc /= 'abcd') error stop 18
    if (pd // '|' /= 'ab  |') error stop 19
    print *, 'ok'
end program
