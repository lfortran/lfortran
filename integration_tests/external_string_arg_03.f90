program external_string_arg_03
    ! Character arguments of procedures without bind(c) are passed like
    ! gfortran does: a pointer to the data in the argument's position, and
    ! the lengths by value after all the arguments, in the order of the
    ! character arguments (#14161). The C procedures in
    ! external_string_arg_03_c.c abort if they receive anything else.
    implicit none
    external show, show_arr, c_call_show, show_f
    character(len=5) :: s
    character :: x(2)
    character(len=4) :: names(3)
    integer :: n
    s = 'Hello'
    n = 42
    x = ['A', 'B']
    names = ['abcd', 'efgh', 'ijkl']

    ! void show_(char *a, int *n, char *b, int64_t la, int64_t lb)
    ! It changes a(1:1) to 'J' and b to 'Z'.
    call show(s, n, x(2))
    if (s /= 'Jello') error stop 1
    if (x(1) /= 'A') error stop 2
    if (x(2) /= 'Z') error stop 3

    ! void show_arr_(char *names, int *n, int64_t len): an array passes
    ! the pointer to its first element and the element length.
    call show_arr(names, size(names))

    ! C calls the Fortran procedure show_f the same way show is called.
    call c_call_show(show_f, n)
    if (n /= 1) error stop 4
    print *, "ok"
end program

subroutine show_f(a, n, b)
    implicit none
    character(len=*) :: a
    integer :: n
    character :: b
    if (len(a) /= 5) error stop 10
    if (a /= 'Hello') error stop 11
    if (n /= 42) error stop 12
    if (len(b) /= 1) error stop 13
    if (b /= 'B') error stop 14
    a(1:1) = 'J'
    b = 'Z'
    n = 1
end subroutine
