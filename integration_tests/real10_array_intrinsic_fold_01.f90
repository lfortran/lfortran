program real10_array_intrinsic_fold_01
    ! Array intrinsics of real(10) and real(16) parameter arrays fold exactly:
    ! elements that differ by less than a double precision ulp stay distinct
    implicit none
    real(10), parameter :: h = 8 * epsilon(1.0_10)
    real(10), parameter :: r(5) = [1.0_10, 1 + h, 1.0_10, 1 + h, 0.5_10]
    real(10), parameter :: z(0) = [real(10) ::]
    integer, parameter :: i_max(1) = maxloc(r)
    integer, parameter :: i_max_back(1) = maxloc(r, back=.true.)
    integer, parameter :: i_min(1) = minloc(r)
    integer, parameter :: i_min_mask(1) = minloc(r, mask=r > 0.75_10)
    integer, parameter :: i_min_mask_back(1) = minloc(r, mask=r > 0.75_10, back=.true.)
    integer, parameter :: i_find(1) = findloc(r, 1 + h)
    integer, parameter :: i_find_back(1) = findloc(r, 1 + h, back=.true.)
    integer, parameter :: i_find_one(1) = findloc(r, 1.0_10)
    integer, parameter :: i_find_none(1) = findloc(r, 1.0_10, &
        mask=[.false., .false., .false., .false., .true.])
    real(10), parameter :: s = sum(r)
    real(10), parameter :: s_mask = sum(r, mask=r > 1)
    real(10), parameter :: s_empty = sum(r, mask=r > 2)
    real(10), parameter :: p_empty = product(r, mask=r > 2)
    real(10), parameter :: s_zero = sum(z)
    real(10), parameter :: p_zero = product(z)
    real(10), parameter :: mx_mask = maxval(r, mask=r < 1)
    real(10), parameter :: mn_mask = minval(r, mask=r > 0.75_10)
    real(10), parameter :: mx_none = maxval(r, mask=r > 2)
    real(10), parameter :: mn_none = minval(r, mask=r > 2)
    real(10), parameter :: sp(2, 2) = spread(r(1:2), 1, 2)
    real(10), parameter :: pk(2) = pack(r, r > 1)
    real(10), parameter :: up(3) = unpack([1 + h, 2.0_10], &
        [.true., .false., .true.], 0.0_10)
    real(10), parameter :: cs(5) = cshift(r, 1)
    real(10), parameter :: eo(5) = eoshift(r, 1)

    real(16), parameter :: hq = 8 * epsilon(1.0_16)
    real(16), parameter :: q(5) = [1.0_16, 1 + hq, 1.0_16, 1 + hq, 0.5_16]
    integer, parameter :: q_max_back(1) = maxloc(q, back=.true.)
    integer, parameter :: q_min_mask(1) = minloc(q, mask=q > 0.75_16)
    integer, parameter :: q_find_back(1) = findloc(q, 1 + hq, back=.true.)
    real(16), parameter :: q_sum = sum(q)
    real(16), parameter :: q_mx_mask = maxval(q, mask=q < 1)
    real(16), parameter :: q_p_empty = product(q, mask=q > 2)

    if (i_max(1) /= 2) error stop "maxloc"
    if (i_max_back(1) /= 4) error stop "maxloc back"
    if (i_min(1) /= 5) error stop "minloc"
    if (i_min_mask(1) /= 1) error stop "minloc mask"
    if (i_min_mask_back(1) /= 3) error stop "minloc mask back"
    if (i_find(1) /= 2) error stop "findloc"
    if (i_find_back(1) /= 4) error stop "findloc back"
    if (i_find_one(1) /= 1) error stop "findloc one"
    if (i_find_none(1) /= 0) error stop "findloc none"
    if (s /= 4.5_10 + 2 * h) error stop "sum"
    if (s_mask /= 2 + 2 * h) error stop "sum mask"
    if (s_empty /= 0) error stop "sum empty mask"
    if (p_empty /= 1) error stop "product empty mask"
    if (s_zero /= 0) error stop "sum zero size"
    if (p_zero /= 1) error stop "product zero size"
    if (mx_mask /= 0.5_10) error stop "maxval mask"
    if (mn_mask /= 1) error stop "minval mask"
    if (mx_none /= -huge(1.0_10)) error stop "maxval mask selects nothing"
    if (mn_none /= huge(1.0_10)) error stop "minval mask selects nothing"
    if (any(sp(:, 1) /= 1) .or. any(sp(:, 2) /= 1 + h)) error stop "spread"
    if (any(pk /= 1 + h)) error stop "pack"
    if (up(1) /= 1 + h .or. up(2) /= 0 .or. up(3) /= 2) error stop "unpack"
    if (any(cs /= [1 + h, 1.0_10, 1 + h, 0.5_10, 1.0_10])) error stop "cshift"
    if (any(eo /= [1 + h, 1.0_10, 1 + h, 0.5_10, 0.0_10])) error stop "eoshift"

    if (q_max_back(1) /= 4) error stop "q maxloc back"
    if (q_min_mask(1) /= 1) error stop "q minloc mask"
    if (q_find_back(1) /= 4) error stop "q findloc back"
    if (q_sum /= 4.5_16 + 2 * hq) error stop "q sum"
    if (q_mx_mask /= 0.5_16) error stop "q maxval mask"
    if (q_p_empty /= 1) error stop "q product empty mask"
    call mask_dim_reductions()
    call dim_reductions()
    print *, i_max, i_max_back, i_find, i_find_back, s

contains

    ! count, any, all and parity with dim= of masks derived from a real(10)
    ! parameter array fold along dim, in named constants and executable
    ! statements
    subroutine mask_dim_reductions()
        real(10), parameter :: m(2, 3) = reshape([1.0_10, -2.0_10, 3.0_10, &
            3.0_10, -5.0_10, 2.0_10], [2, 3])
        integer, parameter :: c(3) = count(m > 0, dim=1)
        integer, parameter :: cr(2) = count(m > 0, dim=2)
        integer(8), parameter :: ci(3) = count(int(m) > 0, dim=1, kind=8)
        logical, parameter :: a(3) = any(m > 2, dim=1)
        logical, parameter :: l(2) = all(m > -3, dim=2)
        logical, parameter :: p(3) = parity(m > 0, dim=1)
        integer :: ce(3)
        logical :: ae(3), le(2), pe(3)
        if (any(c /= [1, 2, 1])) error stop "count dim"
        if (any(cr /= [2, 2])) error stop "count dim 2"
        if (any(ci /= [1_8, 2_8, 1_8])) error stop "count int dim kind"
        if (any(a .neqv. [.false., .true., .false.])) error stop "any dim"
        if (any(l .neqv. [.false., .true.])) error stop "all dim"
        if (any(p .neqv. [.true., .false., .true.])) error stop "parity dim"
        ce = count(m > 0, dim=1)
        if (any(ce /= [1, 2, 1])) error stop "count dim executable"
        ce = count(int(m) > 0, dim=1)
        if (any(ce /= [1, 2, 1])) error stop "count int dim executable"
        ae = any(m > 2, dim=1)
        if (any(ae .neqv. [.false., .true., .false.])) error stop "any dim executable"
        le = all(m > -3, dim=2)
        if (any(le .neqv. [.false., .true.])) error stop "all dim executable"
        pe = parity(m > 0, dim=1)
        if (any(pe .neqv. [.true., .false., .true.])) error stop "parity dim executable"
    end subroutine

    ! sum, product, maxval, minval and norm2 with dim=, and cshift and
    ! eoshift along dim=2, of a rank-2 real(10) parameter array fold exactly,
    ! in named constants and executable statements
    subroutine dim_reductions()
        real(10), parameter :: h = 8 * epsilon(1.0_10)
        real(10), parameter :: m(2, 3) = reshape([1 + h, 2.0_10, 3.0_10, &
            1 + 2 * h, 0.5_10, 3 + 4 * h], [2, 3])
        real(10), parameter :: s1(3) = sum(m, dim=1)
        real(10), parameter :: s2(2) = sum(m, dim=2)
        real(10), parameter :: p1(3) = product(m, dim=1)
        real(10), parameter :: mx1(3) = maxval(m, dim=1)
        real(10), parameter :: mx2(2) = maxval(m, dim=2)
        real(10), parameter :: mn1(3) = minval(m, dim=1)
        real(10), parameter :: mn2(2) = minval(m, dim=2)
        real(10), parameter :: mxm1(3) = maxval(m, dim=1, mask=m < 2)
        real(10), parameter :: mxn1(3) = maxval(m, dim=1, mask=m < 1)
        real(10), parameter :: mnn2(2) = minval(m, dim=2, mask=m > 2.5_10)
        real(10), parameter :: n(2, 2) = reshape([3 * (1 + h), 4 * (1 + h), &
            6 * (1 + h), 8 * (1 + h)], [2, 2])
        real(10), parameter :: n1(2) = norm2(n, dim=1)
        real(10), parameter :: n2(2) = norm2(n, dim=2)
        real(10), parameter :: big(2, 1) = reshape([3.0e4000_10, 4.0e4000_10], [2, 1])
        real(10), parameter :: nb(1) = norm2(big, dim=1)
        real(10), parameter :: cs2(2, 3) = cshift(m, [1, -1], dim=2)
        real(10), parameter :: eo2(2, 3) = eoshift(m, 1, &
            boundary=[9 + 8 * h, 10 + 8 * h], dim=2)
        real(10), parameter :: tol = 4 * epsilon(1.0_10)
        real(10) :: e1(3), e2(2)
        if (any(s1 /= [3 + h, 4 + 2 * h, 3.5_10 + 4 * h])) error stop "sum dim 1"
        if (any(s2 /= [4.5_10 + h, 6 + 6 * h])) error stop "sum dim 2"
        if (any(p1 /= [2 + 2 * h, 3 + 6 * h, 1.5_10 + 2 * h])) error stop "product dim 1"
        if (any(mx1 /= [2.0_10, 3.0_10, 3 + 4 * h])) error stop "maxval dim 1"
        if (any(mx2 /= [3.0_10, 3 + 4 * h])) error stop "maxval dim 2"
        if (any(mn1 /= [1 + h, 1 + 2 * h, 0.5_10])) error stop "minval dim 1"
        if (any(mn2 /= [0.5_10, 1 + 2 * h])) error stop "minval dim 2"
        if (any(mxm1 /= [1 + h, 1 + 2 * h, 0.5_10])) error stop "maxval dim mask"
        if (any(mxn1 /= [-huge(1.0_10), -huge(1.0_10), 0.5_10])) &
            error stop "maxval dim mask selects nothing"
        if (any(mnn2 /= [3.0_10, 3 + 4 * h])) error stop "minval dim mask"
        if (any(abs(n1 - [5 * (1 + h), 10 * (1 + h)]) > tol * [5, 10])) &
            error stop "norm2 dim 1"
        if (any(abs(n2 - sqrt([45.0_10, 80.0_10]) * (1 + h)) > tol * [7, 9])) &
            error stop "norm2 dim 2"
        if (abs(nb(1) - 5.0e4000_10) > tol * 5.0e4000_10) error stop "norm2 dim scaled"
        if (any(cs2(1, :) /= [3.0_10, 0.5_10, 1 + h])) error stop "cshift dim 2 row 1"
        if (any(cs2(2, :) /= [3 + 4 * h, 2.0_10, 1 + 2 * h])) error stop "cshift dim 2 row 2"
        if (any(eo2(1, :) /= [3.0_10, 0.5_10, 9 + 8 * h])) error stop "eoshift dim 2 row 1"
        if (any(eo2(2, :) /= [1 + 2 * h, 3 + 4 * h, 10 + 8 * h])) error stop "eoshift dim 2 row 2"
        e1 = sum(m, dim=1)
        if (any(e1 /= s1)) error stop "sum dim 1 executable"
        e2 = minval(m, dim=2, mask=m > 2.5_10)
        if (any(e2 /= mnn2)) error stop "minval dim mask executable"
        e1 = maxval(m, dim=1, mask=m < 1)
        if (any(e1 /= mxn1)) error stop "maxval dim mask executable"
        e2 = norm2(n, dim=1)
        if (any(e2 /= n1)) error stop "norm2 dim 1 executable"
    end subroutine
end program
