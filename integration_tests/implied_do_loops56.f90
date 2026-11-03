program implied_do_loops56
    ! The variable of an implied DO in an array constructor is a variable of
    ! its own: the variable of the same name outside keeps its value, also
    ! in a contained function that uses it. The variable of an implied DO
    ! in an input/output list is that variable, and keeps its final value.
    implicit none
    integer :: i, j, a(3), b(6)
    character(len=80) :: text

    i = 41
    a = [(f(), i = 1, 3)]
    if (any(a /= 41)) error stop
    if (i /= 41) error stop

    i = 41
    write(text, '(3i4)') [(f(), i = 1, 3)]
    if (text /= '  41  41  41') error stop
    if (i /= 41) error stop

    i = 41
    write(text, '(3i4)') (f(), i = 1, 3)
    if (text /= '   1   2   3') error stop
    if (i /= 4) error stop

    i = 41
    j = 0
    write(text, '(4i4)') ([(i*j, i = 1, 2)], j = 1, 2)
    if (text /= '   1   2   2   4') error stop
    if (i /= 41) error stop
    if (j /= 3) error stop

    i = 41
    write(text, '(6i4)') (i, [(i, i = 1, 2)], i = 5, 6)
    if (text /= '   5   1   2   6   1   2') error stop
    if (i /= 7) error stop

    i = 41
    b = [((10*i + j, j = 1, 2), i = 1, 3)]
    if (any(b /= [11, 12, 21, 22, 31, 32])) error stop
    if (i /= 41) error stop
    print *, "ok"
contains
    integer function f()
        f = i
    end function
end program
