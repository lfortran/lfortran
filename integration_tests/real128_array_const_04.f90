program real128_array_const_04
    ! Reductions of real(16) and complex(16) parameter arrays and of array
    ! expressions in parameter initializers are folded exactly
    implicit none
    real(16), parameter :: m(4) = [1.5_16, 2.0_16, 0.25_16, 4.0_16]
    real(16), parameter :: n(4) = [2.5_16, 1.0_16, 0.5_16, 3.0_16]
    complex(16), parameter :: a(2) = [(1.5_16, 2.25_16), (2.0_16, 1.0_16)]
    complex(16), parameter :: b(2) = [(0.5_16, -1.0_16), (1.0_16, 2.0_16)]
    real(16), parameter :: tiny16 = 1.0e-30_16
    real(16), parameter :: s1 = sum(m * 2)
    real(16), parameter :: s2 = maxval(abs(m - 3))
    real(16), parameter :: s3 = minval(m - 3)
    real(16), parameter :: s4 = product(m + 1)
    real(16), parameter :: s5 = sum(m)
    real(16), parameter :: s6 = dot_product(m, n)
    real(16), parameter :: s7 = dot_product(m * 2, n)
    real(16), parameter :: s8 = sum(m, m > 1)
    real(16), parameter :: s9 = (1.0_16 + 1.0e-20_16) + sum([1.0e-20_16, 1.0e-20_16]) - 1.0_16
    complex(16), parameter :: c1 = sum(a)
    complex(16), parameter :: c2 = product(a)
    complex(16), parameter :: c3 = sum(a * 2)
    complex(16), parameter :: c4 = dot_product(a, b)
    real(16), parameter :: z(2) = [-0.0_16, -0.0_16]
    real(16), parameter :: sz = sum(z)
    real(16), parameter :: pz = product([-0.0_16, 1.0_16])
    real(16) :: d1, d2
    real(16), parameter :: p2 = product(m, m > 1)
    real(16), parameter :: mn2 = minval(m, m > 1)
    real(16), parameter :: e1 = sum(m, m > 100)
    real(16), parameter :: e2 = product(m, m > 100)
    real(16), parameter :: inf = huge(1.0_16) * 2
    real(16), parameter :: i1 = maxval([1.0_16, inf])
    real(16), parameter :: i2 = minval([-inf, 1.0_16])
    real(16) :: mv(4), nv(4)
    complex(16) :: av(2), bv(2)

    d1 = dot_product(m, [1.0_8, 2.0_8, 3.0_8, 4.0_8])
    d2 = dot_product([1.0_8, 2.0_8, 3.0_8, 4.0_8], m)
    mv = m
    nv = n
    av = a
    bv = b

    if (abs(s1 - sum(mv * 2)) > tiny16) error stop 1
    if (abs(s1 - 15.5_16) > tiny16) error stop 2
    if (abs(s2 - maxval(abs(mv - 3))) > tiny16) error stop 3
    if (abs(s2 - 2.75_16) > tiny16) error stop 4
    if (abs(s3 - minval(mv - 3)) > tiny16) error stop 5
    if (abs(s4 - product(mv + 1)) > tiny16) error stop 6
    if (abs(s5 - 7.75_16) > tiny16) error stop 7
    if (abs(s6 - dot_product(mv, nv)) > tiny16) error stop 8
    if (abs(s6 - 17.875_16) > tiny16) error stop 9
    if (abs(s7 - dot_product(mv * 2, nv)) > tiny16) error stop 10
    if (abs(s8 - 7.5_16) > tiny16) error stop 11
    if (abs(s9 - 3.0e-20_16) > 1.0e-33_16) error stop 12

    if (abs(c1 - sum(av)) > tiny16) error stop 13
    if (abs(c2 - product(av)) > tiny16) error stop 14
    if (abs(c3 - sum(av * 2)) > tiny16) error stop 15
    if (abs(c4 - dot_product(av, bv)) > tiny16) error stop 16
    if (abs(c1 - (3.5_16, 3.25_16)) > tiny16) error stop 17
    if (d1 /= dot_product(mv, [1.0_8, 2.0_8, 3.0_8, 4.0_8])) error stop 18
    if (d2 /= d1) error stop 19
    if (abs(d1 - 22.25_16) > tiny16) error stop 20
    if (sign(1.0_16, sz) < 0) error stop 21
    if (sign(1.0_16, pz) > 0) error stop 22
    if (abs(p2 - 12.0_16) > tiny16) error stop 23
    if (abs(mn2 - 1.5_16) > tiny16) error stop 24
    if (e1 /= 0) error stop 25
    if (e2 /= 1) error stop 26
    if (i1 <= huge(1.0_16)) error stop 27
    if (i2 >= -huge(1.0_16)) error stop 28
    print *, s1, s2, c1
end program
