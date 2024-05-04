program intrinsics_217
    implicit none
    logical :: l
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

    l = all([.true., .true., .true.])
    print *, l
    call section

contains
    subroutine section
        integer a(2,3), b(2,3)
        a = 1
        b = 1
        b(2,2) = 2
        print *, all(a == b, 1)
        if (all(all(a == b, 1))) error stop
        print *, all(a == b, 2)
        if (.not. any(all(a == b, 2))) error stop
    end subroutine section
end program
