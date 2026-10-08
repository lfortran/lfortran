module string_arg_hidden_length_04_m
    ! Character dummies get their hidden length arguments even when the
    ! string_length_arguments pass is not among the passes selected
    ! (--skip-pass, --pass), and only once with --cumulative.
    implicit none
contains
    subroutine show(a, n, b, c)
        character(len=*), intent(in) :: a
        integer, intent(in) :: n
        character(len=2), intent(in) :: b
        character(len=*), intent(out) :: c
        if (len(a) /= 5) error stop 1
        if (a /= 'Hello') error stop 2
        if (n /= 42) error stop 3
        if (b /= 'el') error stop 4
        if (len(c) /= 3) error stop 5
        c = a(1:1) // b
    end subroutine
end module

program string_arg_hidden_length_04
    use string_arg_hidden_length_04_m
    implicit none
    character(len=5) :: s = 'Hello'
    character(len=3) :: r
    call show(s, 42, s(2:3), r)
    if (r /= 'Hel') error stop 6
    print *, r
end program
