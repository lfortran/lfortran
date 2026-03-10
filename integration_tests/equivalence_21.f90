subroutine test_reversed_order()
    integer :: j1, j2
    common /block2/ j1
    equivalence (j1, j2)

    j1 = 10
    j2 = 99
    if (j1 /= 99) error stop
    if (j2 /= 99) error stop
end subroutine

program equivalence_21
    equivalence (i1, i2)
    common /block/ i1

    i1 = 24
    i2 = 42
    if (i1 /= 42) error stop
    if (i2 /= 42) error stop

    call test_reversed_order()
end program