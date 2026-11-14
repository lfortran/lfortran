module substring_section_arg_01_m
    implicit none
    interface set_generic
        module procedure set_inout
    end interface
    type :: substring_section_arg_01_t
    contains
        procedure :: set_bound
        generic :: set => set_bound
    end type
contains
    subroutine set_inout(c)
        character(*), intent(inout) :: c(:)
        c = 'aa'
    end subroutine

    subroutine set_out(c)
        character(*), intent(out) :: c(:)
        c = 'bb'
    end subroutine

    subroutine set_no_intent(c)
        character(*) :: c(*)
        c(1:2)(1:1) = '#'
    end subroutine

    elemental subroutine set_elemental(c)
        character(*), intent(inout) :: c
        c(1:1) = 'c'
    end subroutine

    subroutine join_no_intent(c, r)
        character(*) :: c(:), r
        r = c(1) // c(2)
    end subroutine

    subroutine join_intent_in(a, r)
        character(*), intent(in) :: a(:)
        character(*), intent(out) :: r
        call join_no_intent(a(2:3)(1:2), r)
    end subroutine

    subroutine set_bound(this, c)
        class(substring_section_arg_01_t), intent(in) :: this
        character(*), intent(inout) :: c(:)
        c = 'dd'
    end subroutine
end module

program substring_section_arg_01
    use substring_section_arg_01_m
    implicit none
    character(len=3) :: e(4)
    character(len=4) :: r
    type(substring_section_arg_01_t) :: o

    e = ['xxx', 'yyy', 'zzz', 'www']
    call set_inout(e(2:4:2)(1:2))
    print *, e
    if (any(e /= ['xxx', 'aay', 'zzz', 'aaw'])) error stop 1

    e = ['xxx', 'yyy', 'zzz', 'www']
    call set_out(e(:)(2:3))
    print *, e
    if (any(e /= ['xbb', 'ybb', 'zbb', 'wbb'])) error stop 2

    e = ['xxx', 'yyy', 'zzz', 'www']
    call set_no_intent(e(1:2)(2:3))
    print *, e
    if (any(e /= ['x#x', 'y#y', 'zzz', 'www'])) error stop 3

    e = ['xxx', 'yyy', 'zzz', 'www']
    call set_elemental(e(2:4:2)(1:2))
    print *, e
    if (any(e /= ['xxx', 'cyy', 'zzz', 'cww'])) error stop 4

    e = ['xxx', 'yyy', 'zzz', 'www']
    call set_generic(e(1:3:2)(2:3))
    print *, e
    if (any(e /= ['xaa', 'yyy', 'zaa', 'www'])) error stop 5

    call join_intent_in(['abc', 'def', 'ghi'], r)
    print *, r
    if (r /= 'degh') error stop 6

    e = ['xxx', 'yyy', 'zzz', 'www']
    call o%set(e(2:3)(2:3))
    print *, e
    if (any(e /= ['xxx', 'ydd', 'zdd', 'www'])) error stop 7
end program
