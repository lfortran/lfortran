program array_constructor_16
    ! Array constructors combining scalars with an implied-do loop whose
    ! end bound is an integer(8), compiled with --descriptor-index-64, where
    ! `size` also returns an integer(8). The size of the constructor is
    ! computed from the trip count converted to the kind of the other sizes.
    implicit none
    real :: x(4), r(5)
    integer :: i, row, a(3)
    integer(8) :: n8

    x = [1.0, 2.0, 3.0, 4.0]
    n8 = 2

    ! end bound computed from size() between scalars
    r = [x(1), (x(row), row = 1, size(x) - 1), x(4)]
    if (any(abs(r - [1.0, 1.0, 2.0, 3.0, 4.0]) > 1e-6)) error stop 1

    ! integer(8) variable end bound next to a scalar
    a = [0, (i, i = 1, n8)]
    if (any(a /= [0, 1, 2])) error stop 2

    ! integer(8) constant end bound between scalars
    r = [x(1), (x(row), row = 1, 3_8), x(4)]
    if (any(abs(r - [1.0, 1.0, 2.0, 3.0, 4.0]) > 1e-6)) error stop 3

    print *, a, r
end program
