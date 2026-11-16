module complex10_param_fold_03_mod
    implicit none
    complex(10), parameter :: mz = (1.0_10, 2.0_10)
    complex(10) :: m_mul(2) = mz*2
    complex(10) :: m_lit(2) = [mz, mz]
    complex(8) :: m8_mul(2) = [mz*2, mz]
    complex(10) :: m_rs(2, 2) = reshape([mz, mz, mz, -mz], [2, 2])
    complex(10) :: m_rr(4) = reshape(reshape([mz, mz, mz, -mz], [2, 2]), [4])
    real(10) :: mr_rr(4) = reshape(reshape([1.0_10, 2.0_10, 3.0_10, 4.0_10], &
        [2, 2]), [4])
end module

program complex10_param_fold_03
    ! Array intrinsics, conversions and nested array constructors of
    ! complex(10) array constructors in constant expressions, and complex(10)
    ! scalar constants broadcast to arrays, including values from folded
    ! complex(10) arithmetic, and reshapes of complex(10) array constructors
    use complex10_param_fold_03_mod, only: m_mul, m_lit, m8_mul, m_rs, m_rr, &
        mr_rr
    implicit none
    complex(10), parameter :: z = (1.0_10, 2.0_10)
    type :: t
        complex(10) :: d(2) = z*2
    end type
    type(t) :: x
    complex(10), parameter :: s_neg = sum([-z, z])
    complex(10), parameter :: s_mul = sum([z*2])
    complex(10), parameter :: s_mix = sum([z, (1.0_10, 0.0_10)*z])
    complex(10), parameter :: s_lit = sum([z, z])
    complex(10), parameter :: p_mul = product([z*2, z])
    complex(10), parameter :: p_lit = product([z, z])
    complex(10), parameter :: d_mul = dot_product([z*2], [z])
    complex(10), parameter :: d_lit = dot_product([z], [z])
    complex(10), parameter :: k_mul(1) = pack([z*2, z], [.true., .false.])
    complex(10), parameter :: k_lit(1) = pack([z, z], [.false., .true.])
    complex(8), parameter :: c8_mul(2) = [z*2, z]
    complex(4), parameter :: c4_neg(2) = [-z, z]
    complex(16), parameter :: c16_mul(2) = [z*2, z]
    complex(10), parameter :: n_mul(3) = [[z*2, z], z]
    complex(10), parameter :: t_mul(2) = [complex(10) :: z*2, (1.0d0, 3.0d0)]
    complex(10), parameter :: b_neg(2) = -z
    complex(10), parameter :: b_add(2) = z + z
    complex(10), parameter :: b_lit(2) = z
    complex(10), parameter :: b_conjg(2) = conjg(z)
    complex(10), parameter :: b_2d(2, 2) = z*2
    complex(8), parameter :: b8_mul(2) = z*2
    complex(10) :: v_mul(2) = z*2
    real(10), parameter :: r_aimag(2) = aimag(z*2)
    complex(10), parameter :: rs_lit(2, 2) = reshape([z, z, z, z], [2, 2])
    complex(10), parameter :: rs_mul(2, 2) = reshape([z*2, z, -z, z], [2, 2])
    complex(10), parameter :: rs_con(2, 2) = reshape([(1.0_10, 2.0_10), &
        (3.0_10, 4.0_10), (5.0_10, 6.0_10), (7.0_10, 8.0_10)], [2, 2])
    complex(10) :: rv_mul(2, 2) = reshape([z*2, z, -z, z], [2, 2])

    if (s_neg /= (0.0_10, 0.0_10)) error stop 1
    if (s_mul /= (2.0_10, 4.0_10)) error stop 2
    if (s_mix /= (2.0_10, 4.0_10)) error stop 3
    if (s_lit /= (2.0_10, 4.0_10)) error stop 4
    if (p_mul /= (-6.0_10, 8.0_10)) error stop 5
    if (p_lit /= (-3.0_10, 4.0_10)) error stop 6
    if (d_mul /= (10.0_10, 0.0_10)) error stop 7
    if (d_lit /= (5.0_10, 0.0_10)) error stop 8
    if (size(k_mul) /= 1) error stop 9
    if (k_mul(1) /= (2.0_10, 4.0_10)) error stop 10
    if (k_lit(1) /= (1.0_10, 2.0_10)) error stop 11
    if (any(c8_mul /= [(2.0_8, 4.0_8), (1.0_8, 2.0_8)])) error stop 12
    if (any(c4_neg /= [(-1.0_4, -2.0_4), (1.0_4, 2.0_4)])) error stop 13
    if (any(c16_mul /= [(2.0_16, 4.0_16), (1.0_16, 2.0_16)])) error stop 14
    if (any(n_mul /= [(2.0_10, 4.0_10), (1.0_10, 2.0_10), (1.0_10, 2.0_10)])) error stop 15
    if (any(t_mul /= [(2.0_10, 4.0_10), (1.0_10, 3.0_10)])) error stop 16
    if (any(b_neg /= (-1.0_10, -2.0_10))) error stop 17
    if (any(b_add /= (2.0_10, 4.0_10))) error stop 18
    if (any(b_lit /= (1.0_10, 2.0_10))) error stop 19
    if (any(b_conjg /= (1.0_10, -2.0_10))) error stop 20
    if (any(b_2d /= (2.0_10, 4.0_10))) error stop 21
    if (any(b8_mul /= (2.0_8, 4.0_8))) error stop 22
    if (any(v_mul /= (2.0_10, 4.0_10))) error stop 23
    if (any(r_aimag /= 4.0_10)) error stop 24
    if (any(x%d /= (2.0_10, 4.0_10))) error stop 25
    if (any(m_mul /= (2.0_10, 4.0_10))) error stop 26
    if (any(m_lit /= (1.0_10, 2.0_10))) error stop 27
    if (any(m8_mul /= [(2.0_8, 4.0_8), (1.0_8, 2.0_8)])) error stop 28
    if (any(rs_lit /= (1.0_10, 2.0_10))) error stop 29
    if (rs_mul(1, 1) /= (2.0_10, 4.0_10) .or. rs_mul(2, 1) /= z &
        .or. rs_mul(1, 2) /= -z .or. rs_mul(2, 2) /= z) error stop 30
    if (rs_con(1, 1) /= (1.0_10, 2.0_10) .or. rs_con(2, 1) /= (3.0_10, 4.0_10) &
        .or. rs_con(1, 2) /= (5.0_10, 6.0_10) &
        .or. rs_con(2, 2) /= (7.0_10, 8.0_10)) error stop 31
    if (any(rv_mul /= rs_mul)) error stop 32
    if (any(m_rs(:, 1) /= z) .or. m_rs(1, 2) /= z .or. m_rs(2, 2) /= -z) error stop 33
    if (any(m_rr(1:3) /= z) .or. m_rr(4) /= -z) error stop 34
    if (any(mr_rr /= [1.0_10, 2.0_10, 3.0_10, 4.0_10])) error stop 35
    print *, s_neg, s_mul, p_mul, d_mul, k_mul
    print *, c8_mul, n_mul, b_neg, b_2d, v_mul
end program
