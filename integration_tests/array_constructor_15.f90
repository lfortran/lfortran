program array_constructor_15
    ! Array constructors combining other elements with an implied-do loop
    ! whose end bound is not a default integer, unlike its loop variable,
    ! or whose loop variable and bounds are all integer(8) (see
    ! array_constructor_16 for --descriptor-index-64). The size of the
    ! constructor is computed from the trip count in the widest kind of the
    ! bounds, converted to the kind of the other sizes. array_constructor_17
    ! runs this file with -fdefault-integer-8.
    implicit none
    real :: x(4), r(5)
    integer :: i, j, row
    integer :: a(3), c(6), t(7), u(10)
    integer(8) :: n8, k8
    integer(2) :: n2

    x = [1.0, 2.0, 3.0, 4.0]
    n8 = 2
    n2 = 2

    ! integer(8) variable end bound next to a scalar
    a = [0, (i, i = 1, n8)]
    if (any(a /= [0, 1, 2])) error stop 1

    ! integer(2) variable end bound next to a scalar
    a = [0, (i, i = 1, n2)]
    if (any(a /= [0, 1, 2])) error stop 2

    ! integer(8) constant end bound between scalars
    r = [x(1), (x(row), row = 1, 3_8), x(4)]
    if (any(abs(r - [1.0, 1.0, 2.0, 3.0, 4.0]) > 1e-6)) error stop 3

    ! nested implied-do with an integer(8) inner end bound
    c = [((i*10 + j, j = 1, n8 + 1), i = 1, 2)]
    if (any(c /= [11, 12, 13, 21, 22, 23])) error stop 4

    ! triangular nested implied-do with an integer(8) outer end bound
    n8 = 3
    t = [0, ((j, j = 1, i), i = 1, n8)]
    if (size(t) /= 7) error stop 5
    if (any(t /= [0, 1, 1, 2, 1, 2, 3])) error stop 6
    if (size([0, ((j, j = 1, i), i = 1, n8)]) /= 7) error stop 7

    ! the same with an element of the outer implied-do before the inner one
    u = [0, (i, (j, j = 1, i), i = 1, n8)]
    if (size(u) /= 10) error stop 8
    if (any(u /= [0, 1, 1, 2, 1, 2, 3, 1, 2, 3])) error stop 9
    if (size([0, (i, (j, j = 1, i), i = 1, n8)]) /= 10) error stop 10

    ! integer(8) loop variable and bounds between scalars
    n8 = 2
    x = [1.0, (real(k8 + 2), k8 = 1_8, n8), 2.0]
    if (any(abs(x - [1.0, 3.0, 4.0, 2.0]) > 1e-6)) error stop 11
    if (size([1.0, (real(k8 + 2), k8 = 1_8, n8), 2.0]) /= 4) error stop 12

    print *, a, r, c, t, u, x
end program
