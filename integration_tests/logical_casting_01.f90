program logical_casting_01
    implicit none
    logical :: x(3) = [1, 1, 0]
    print *, x
    if (x(1) .neqv. .true.) error stop
    if (x(2) .neqv. .true.) error stop
    if (x(3) .neqv. .false.) error stop
end program