program associate_64
    ! An array converted to the type of an associate name or a pointer
    ! (intrinsic assignment, F2018 10.2.1.3, and mixed-type operations)
    implicit none
    real(8) :: r(2), d(2)
    real(4) :: s(2)
    integer :: i(2)
    complex(8), target :: z(2)
    complex(8), pointer :: zp(:)

    r = [2d0, 3d0]
    associate (p => z)
        p = r
    end associate
    print *, z
    if (any(z /= [(2d0, 0d0), (3d0, 0d0)])) error stop 1

    z = [(1d0, 1d0), (2d0, -1d0)]
    associate (p => z)
        p = r * p
    end associate
    print *, z
    if (any(z /= [(2d0, 2d0), (6d0, -3d0)])) error stop 2

    associate (p => z)
        p = p * r
    end associate
    print *, z
    if (any(z /= [(4d0, 4d0), (18d0, -9d0)])) error stop 3

    associate (p => z)
        p = r + p
    end associate
    print *, z
    if (any(z /= [(6d0, 4d0), (21d0, -9d0)])) error stop 4

    associate (p => z(2:2))
        p = r(1:1) - p
    end associate
    print *, z
    if (any(z /= [(6d0, 4d0), (-19d0, 9d0)])) error stop 5

    i = [4, 5]
    associate (p => z)
        p = i
    end associate
    print *, z
    if (any(z /= [(4d0, 0d0), (5d0, 0d0)])) error stop 6

    s = [1.0, 2.0]
    d = 0d0
    associate (q => d)
        q = q + s
    end associate
    print *, d
    if (any(d /= [1d0, 2d0])) error stop 7

    ! the same conversions through a pointer variable
    zp => z
    zp = r
    zp = r * zp
    print *, z
    if (any(z /= [(4d0, 0d0), (9d0, 0d0)])) error stop 8
end program associate_64
