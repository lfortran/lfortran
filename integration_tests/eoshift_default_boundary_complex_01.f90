program eoshift_default_boundary_complex_01
    ! eoshift of a complex array without BOUNDARY fills with (0, 0) (#16933)
    implicit none
    complex, parameter :: p4(4) = [(1.0, -1.0), (2.0, -2.0), (3.0, -3.0), (4.0, -4.0)]
    complex, parameter :: q4(4) = eoshift(p4, 1)
    complex(8), parameter :: p8(4) = [(1.0d0, -1.0d0), (2.0d0, -2.0d0), &
        (3.0d0, -3.0d0), (4.0d0, -4.0d0)]
    complex(8), parameter :: q8(4) = eoshift(p8, -2)
    complex, parameter :: pm(2, 3) = reshape([(1.0, 1.0), (2.0, 2.0), (3.0, 3.0), &
        (4.0, 4.0), (5.0, 5.0), (6.0, 6.0)], [2, 3])
    complex :: a(4), r(4), m(2, 3), rm(2, 3)
    complex(8) :: d(4), rd(4), md(2, 3), rmd(2, 3)
    integer :: s

    ! rank 1, complex(4), variable, positive and negative shift
    a = p4
    r = eoshift(a, 1)
    if (any(r /= [(2.0, -2.0), (3.0, -3.0), (4.0, -4.0), (0.0, 0.0)])) error stop 1
    r = eoshift(a, -2)
    if (any(r /= [(0.0, 0.0), (0.0, 0.0), (1.0, -1.0), (2.0, -2.0)])) error stop 2

    ! rank 1, complex(8), variable array and variable shift
    d = p8
    s = 3
    rd = eoshift(d, s)
    if (any(rd /= [(4.0d0, -4.0d0), (0.0d0, 0.0d0), (0.0d0, 0.0d0), (0.0d0, 0.0d0)])) error stop 3
    s = -1
    rd = eoshift(d, s)
    if (any(rd /= [(0.0d0, 0.0d0), (1.0d0, -1.0d0), (2.0d0, -2.0d0), (3.0d0, -3.0d0)])) error stop 4

    ! rank 1, PARAMETER folded at compile time
    if (any(q4 /= [(2.0, -2.0), (3.0, -3.0), (4.0, -4.0), (0.0, 0.0)])) error stop 5
    if (any(q8 /= [(0.0d0, 0.0d0), (0.0d0, 0.0d0), (1.0d0, -1.0d0), (2.0d0, -2.0d0)])) error stop 6
    r = eoshift(p4, -1)
    if (any(r /= [(0.0, 0.0), (1.0, -1.0), (2.0, -2.0), (3.0, -3.0)])) error stop 7

    ! rank 2, complex(4), variable, dim=2 and dim=1
    m = pm
    rm = eoshift(m, 1, dim=2)
    if (any(rm(:, 1) /= [(3.0, 3.0), (4.0, 4.0)])) error stop 8
    if (any(rm(:, 2) /= [(5.0, 5.0), (6.0, 6.0)])) error stop 9
    if (any(rm(:, 3) /= [(0.0, 0.0), (0.0, 0.0)])) error stop 10
    rm = eoshift(m, -1, dim=2)
    if (any(rm(:, 1) /= [(0.0, 0.0), (0.0, 0.0)])) error stop 11
    if (any(rm(:, 2) /= [(1.0, 1.0), (2.0, 2.0)])) error stop 12
    if (any(rm(:, 3) /= [(3.0, 3.0), (4.0, 4.0)])) error stop 13
    rm = eoshift(m, 1, dim=1)
    if (any(rm(1, :) /= [(2.0, 2.0), (4.0, 4.0), (6.0, 6.0)])) error stop 14
    if (any(rm(2, :) /= [(0.0, 0.0), (0.0, 0.0), (0.0, 0.0)])) error stop 15

    ! rank 2, complex(4), PARAMETER argument
    rm = eoshift(pm, -1, dim=2)
    if (any(rm(:, 1) /= [(0.0, 0.0), (0.0, 0.0)])) error stop 16
    if (any(rm(:, 2) /= [(1.0, 1.0), (2.0, 2.0)])) error stop 17
    if (any(rm(:, 3) /= [(3.0, 3.0), (4.0, 4.0)])) error stop 18

    ! rank 2, complex(8), variable, dim=2
    md = pm
    rmd = eoshift(md, -2, dim=2)
    if (any(rmd(:, 1) /= [(0.0d0, 0.0d0), (0.0d0, 0.0d0)])) error stop 19
    if (any(rmd(:, 2) /= [(0.0d0, 0.0d0), (0.0d0, 0.0d0)])) error stop 20
    if (any(rmd(:, 3) /= [(1.0d0, 1.0d0), (2.0d0, 2.0d0)])) error stop 21

    print *, "ok"
end program
