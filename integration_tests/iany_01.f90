program iany_01
    ! iany, iall and iparity honour the `mask` argument, whether it is
    ! passed by keyword, positionally (as the second argument), or with `dim`
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    logical, parameter :: T = .true., F = .false.
    integer, parameter :: p_any = iany([36, 106, 170], mask=[T, F, T])
    integer, parameter :: p_all = iall([36, 106, 170], [T, F, T])
    integer, parameter :: p_par = iparity([36, 106, 170], dim=1, mask=[T, F, T])
    integer(int8) :: a(3)
    integer :: b(2, 3), r(3), s(2)
    logical :: m(2, 3), l

    a = [int(b'00100100', int8), int(b'01101010', int8), int(b'10101010', int8)]

    if (iany(a) /= int(b'11101110', int8)) error stop 1
    if (iany(a, mask=[T, F, T]) /= int(b'10101110', int8)) error stop 2
    if (iany(a, [T, F, T]) /= int(b'10101110', int8)) error stop 3
    if (iany(a, dim=1, mask=[T, F, T]) /= int(b'10101110', int8)) error stop 4
    if (iany(a, 1, [T, F, T]) /= iany([a(1), a(3)])) error stop 5
    if (iany(a, mask=[F, F, F]) /= 0) error stop 6

    if (iall(a, mask=[T, F, T]) /= int(b'00100000', int8)) error stop 7
    if (iall(a, [F, T, T]) /= int(b'00101010', int8)) error stop 8
    if (iall(a, dim=1, mask=[T, F, T]) /= iand(a(1), a(3))) error stop 9
    if (iall(a, mask=[F, F, F]) /= -1) error stop 10

    if (iparity(a, mask=[T, F, T]) /= ieor(a(1), a(3))) error stop 11
    if (iparity(a, [T, F, T]) /= ieor(a(1), a(3))) error stop 12

    l = .false.
    if (iany(a, mask=l) /= 0) error stop 13
    l = .true.
    if (iall(a, mask=l) /= iand(iand(a(1), a(2)), a(3))) error stop 14

    if (p_any /= 174) error stop 15
    if (p_all /= 32) error stop 16
    if (p_par /= 142) error stop 17

    b = reshape([1, 2, 4, 8, 16, 32], [2, 3])
    m = reshape([T, F, F, T, T, T], [2, 3])
    r = iany(b, dim=1)
    if (any(r /= [3, 12, 48])) error stop 18
    r = iany(b, dim=1, mask=m)
    if (any(r /= [1, 8, 48])) error stop 19
    s = iany(b, 2, m)
    if (any(s /= [17, 40])) error stop 20
    if (iany(b, m) /= 57) error stop 21
    r = iall(b + 1, dim=1, mask=m)
    if (any(r /= [2, 9, 1])) error stop 22
    print *, "ok"
end program iany_01
