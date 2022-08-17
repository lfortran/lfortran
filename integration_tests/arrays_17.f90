program arrays_17
    integer, parameter :: real_kinds(2) = [4, 8]
    integer :: real_kinds2(2) = [4, 8]
    print *, size(real_kinds)
    print *, real_kinds
    if (real_kinds(1) /= 4) error stop
    if (real_kinds(2) /= 8) error stop

    print *, size(real_kinds2)
    print *, real_kinds2
    if (real_kinds2(1) /= 4) error stop
    if (real_kinds2(2) /= 8) error stop
end program
