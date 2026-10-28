program volatile_06
    ! VOLATILE statement on implicitly typed variables (compiled with
    ! --implicit-typing): `i` is never declared, `k` is declared after the
    ! statement. Both must get the VOLATILE attribute.
    volatile :: i
    volatile :: k
    integer :: k
    i = 3
    k = 4
    i = i + k
    if (i /= 7) error stop
    if (k /= 4) error stop
    print *, i, k
end program volatile_06
