! Subscript triplets whose bounds and stride have different integer kinds
program array_section_39
    implicit none
    integer(1) :: i1
    integer(2) :: i2
    integer :: i4, n
    integer(8) :: i8
    integer(8) :: v8(2)
    integer :: a(10), b(4, 5)
    integer, target :: t(6)
    integer, pointer :: p(:)
    integer :: i

    a = [(i, i = 1, 10)]
    b = reshape([(i, i = 1, 20)], [4, 5])
    i1 = 2_1
    i2 = 3_2
    i4 = 4
    i8 = 9_8
    n = 10
    v8 = [3_8, 7_8]

    if (any(a(1_8:n) /= a)) error stop 1
    if (any(a(i2:i8:1) /= [3, 4, 5, 6, 7, 8, 9])) error stop 2
    if (any(a(i1:i8:i2) /= [2, 5, 8])) error stop 3
    if (any(a(i8:i1:-i2) /= [9, 6, 3])) error stop 4
    if (any(a(i1:i4) /= [2, 3, 4])) error stop 5
    if (any(a(i2:) /= [3, 4, 5, 6, 7, 8, 9, 10])) error stop 6
    if (any(a(:i8:i1) /= [1, 3, 5, 7, 9])) error stop 7
    if (any(a(1_1:4_2:2_8) /= [1, 3])) error stop 8
    if (any(a(v8) /= [3, 7])) error stop 9
    if (any(reshape(b(i1:i8 - 5, i2:i4), [6]) /= [10, 11, 12, 14, 15, 16])) error stop 10
    if (any(b(i2, i1:i8 - 4_8:i2) /= [7, 19])) error stop 11
    a(i1:i8:i4) = 0
    if (any(a /= [1, 0, 3, 4, 5, 0, 7, 8, 9, 10])) error stop 12

    t = [(i, i = 1, 6)]
    p(i2:) => t
    if (lbound(p, 1) /= 3 .or. ubound(p, 1) /= 8) error stop 13
    p(i1:i8 - 4_8) => t
    if (lbound(p, 1) /= 2 .or. ubound(p, 1) /= 5) error stop 14
    if (any(p /= [1, 2, 3, 4])) error stop 15
    print *, "ok"
end program
