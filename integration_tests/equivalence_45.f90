program equivalence_45
    ! EQUIVALENCE must preserve non-default integer kinds on the
    ! paths not covered by equivalence_44 (which covers the
    ! array-element <-> array-element form).
    implicit none
    ! Array element <-> scalar, element first.
    integer(8) :: a1(2), s1
    equivalence (a1(1), s1)
    ! Scalar <-> array element, scalar first.
    integer(8) :: a2(2), s2
    equivalence (s2, a2(1))
    ! Whole array <-> whole array (no subscripts).
    integer(8) :: b1(2), b2(2)
    equivalence (b1, b2)
    ! Whole array <-> scalar (no subscripts).
    integer(8) :: c1(2), s3
    equivalence (c1, s3)
    ! Whole array <-> array element.
    integer(8) :: d1(2), d2(2)
    equivalence (d1, d2(1))

    a1(1) = 4294967297_8
    if (s1 /= 4294967297_8) error stop "element-scalar"

    s2 = 4294967297_8
    if (a2(1) /= 4294967297_8) error stop "scalar-element"

    b2(1) = 4294967297_8
    if (b1(1) /= 4294967297_8) error stop "array-array"

    c1(1) = 4294967297_8
    if (s3 /= 4294967297_8) error stop "array-scalar"

    d2(1) = 4294967297_8
    if (d1(1) /= 4294967297_8) error stop "array-element"
end program
