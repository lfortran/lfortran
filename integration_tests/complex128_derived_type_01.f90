program complex128_derived_type_01
    implicit none
    integer, parameter :: qp = selected_real_kind(33)

    type :: point
        complex(qp) :: z
        integer :: tag
        complex(qp) :: arr(2)
    end type

    type :: holder
        type(point) :: p
        complex(qp), allocatable :: dyn(:)
        complex(qp) :: init = (1.5_qp, -0.5_qp)
    end type

    type(point) :: p, q
    type(point) :: ps(2)
    type(holder) :: h
    complex(qp) :: z
    integer :: i

    p%z = (1.25_qp, 2.5_qp)
    p%tag = 7
    p%arr = [(1, 2), (3, 4)]
    if (p%z /= (1.25_qp, 2.5_qp)) error stop "component"
    if (p%tag /= 7) error stop "neighbour component"
    if (p%arr(2) /= (3, 4)) error stop "array component"

    q = p
    p%z = 0
    if (q%z /= (1.25_qp, 2.5_qp)) error stop "copy"
    if (q%arr(1) /= (1, 2)) error stop "copy array"

    q = point(cmplx(-1, 1, kind=qp), 3, [cmplx(5, 6, kind=qp), cmplx(7, 8, kind=qp)])
    if (q%z /= (-1, 1) .or. q%tag /= 3) error stop "constructor"
    if (sum(q%arr) /= (12, 14)) error stop "constructor array"

    ps(1) = p
    ps(2) = q
    if (ps(2)%arr(2) /= (7, 8)) error stop "array of type"
    z = ps(1)%z + ps(2)%z
    if (z /= (-1, 1)) error stop "sum over array of type"
    ps%z = ps%z * 2
    if (ps(2)%z /= (-2, 2)) error stop "elemental component"

    if (h%init /= (1.5_qp, -0.5_qp)) error stop "default init"
    h%p = q
    allocate(h%dyn(3))
    h%dyn = [(cmplx(i, 0, kind=qp), i = 1, 3)]
    h%dyn(2) = h%dyn(2) * (0, 1)
    if (h%dyn(2) /= (0, 2)) error stop "allocatable component"
    if (h%p%z /= (-1, 1)) error stop "nested"
    h%p%z = h%p%z * h%init
    if (abs(h%p%z - ((-1, 1) * (1.5_qp, -0.5_qp))) > 1.0e-30_qp) error stop "nested update"
    print *, "ok"
end program
