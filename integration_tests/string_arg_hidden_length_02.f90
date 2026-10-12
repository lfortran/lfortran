program string_arg_hidden_length_02
    ! Character arguments of external procedures with an implicit interface,
    ! defined in string_arg_hidden_length_02b.f90 and compiled separately.
    implicit none
    external ext_scalars, ext_array, ext_apply, local_len
    integer, external :: ext_len
    character(len=5) :: s
    character :: c
    character(len=2) :: pairs(3)
    integer :: r
    s = 'Hello'
    call ext_scalars(s, 5, 'xyz', c)
    if (s /= 'Jello') error stop 1
    if (c /= 'y') error stop 2
    call ext_scalars(s(2:4), 3, 'xyz', c)
    if (s /= 'JJllo') error stop 3

    pairs = ['ab', 'cd', 'ef']
    call ext_array(pairs, 3)
    if (pairs(1) /= 'ba' .or. pairs(2) /= 'dc' .or. pairs(3) /= 'fe') error stop 4

    if (ext_len('four') /= 4) error stop 5
    if (ext_len(s // s) /= 10) error stop 6

    call ext_apply(local_len, 'seven!!', r)
    if (r /= 7) error stop 7
    print *, "ok"
end program

subroutine local_len(s, r)
    implicit none
    character(len=*), intent(in) :: s
    integer, intent(out) :: r
    r = len(s)
end subroutine
