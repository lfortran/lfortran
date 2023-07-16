program intrinsics_59
    implicit none

    logical :: a(3) = [.true., .true., .false.]
    logical :: x(2, 2)

    print *, all(a)
    if (all(a)) error stop

    a = [.true., .true., .true.]
    print *, all(a)
    if (.not. all(a)) error stop

    x(1, 1) = .true.
    x(1, 2) = .true.
    x(2, 1) = .true.
    x(2, 2) = .false.

    print *, all(x)
    if (all(x)) error stop
end program
