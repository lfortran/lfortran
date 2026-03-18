! Test: TYPE(*), DIMENSION(..) — assumed-type with assumed-rank
!
! Features tested:
!   - TYPE(*), dimension(..) with integer scalars and rank 1-3 arrays
!   - TYPE(*), dimension(..) with real scalars and arrays
!   - TYPE(*), dimension(..) with BIND(C) derived type
!   - Verifying rank, elem_len, shape, and data content on C side
!   - OPTIONAL + TYPE(*), dimension(..)

program bindc_44
    use iso_c_binding
    implicit none

    type, bind(C) :: pair_t
        integer(c_int32_t) :: a, b
    end type

    interface
        integer(c_int) function c44_get_rank(a) bind(C)
            import :: c_int
            type(*), dimension(..), intent(in) :: a
        end function

        integer(c_int) function c44_get_elem_len(a) bind(C)
            import :: c_int
            type(*), dimension(..), intent(in) :: a
        end function

        integer(c_int) function c44_total_size(a) bind(C)
            import :: c_int
            type(*), dimension(..), intent(in) :: a
        end function

        integer(c_int) function c44_sum_star(a) bind(C)
            import :: c_int
            type(*), dimension(..), intent(in) :: a
        end function

        integer(c_int) function c44_opt_rank(a) bind(C)
            import :: c_int
            type(*), dimension(..), optional, intent(in) :: a
        end function
    end interface

    integer(c_int32_t) :: is
    integer(c_int32_t) :: i1(5)
    integer(c_int32_t) :: i2(3, 4)
    integer(c_int32_t) :: i3(2, 3, 2)
    real(c_float) :: rs
    real(c_float) :: r1(4)
    type(pair_t) :: dt1(3)
    integer :: i

    ! Initialize data
    is = 42
    i1 = [1, 2, 3, 4, 5]
    i2 = reshape([(i, i = 1, 12)], [3, 4])
    i3 = reshape([(i, i = 10, 21)], [2, 3, 2])
    rs = 7.0_c_float
    r1 = [1.0_c_float, 2.0_c_float, 3.0_c_float, 4.0_c_float]
    dt1(1) = pair_t(1, 2)
    dt1(2) = pair_t(3, 4)
    dt1(3) = pair_t(5, 6)

    ! ---- Rank detection for scalar through rank-3 ----
    if (c44_get_rank(is) /= 0) error stop "FAIL: int scalar rank"
    if (c44_get_rank(i1) /= 1) error stop "FAIL: int 1D rank"
    if (c44_get_rank(i2) /= 2) error stop "FAIL: int 2D rank"
    if (c44_get_rank(i3) /= 3) error stop "FAIL: int 3D rank"
    if (c44_get_rank(rs) /= 0) error stop "FAIL: real scalar rank"
    if (c44_get_rank(r1) /= 1) error stop "FAIL: real 1D rank"
    if (c44_get_rank(dt1) /= 1) error stop "FAIL: struct 1D rank"

    ! ---- Element length ----
    if (c44_get_elem_len(is) /= 4)  error stop "FAIL: int32 elem_len"
    if (c44_get_elem_len(rs) /= 4)  error stop "FAIL: float elem_len"
    if (c44_get_elem_len(dt1) /= 8) error stop "FAIL: pair_t elem_len"

    ! ---- Total element count ----
    if (c44_total_size(is) /= 1)  error stop "FAIL: scalar size"
    if (c44_total_size(i1) /= 5)  error stop "FAIL: 1D size"
    if (c44_total_size(i2) /= 12) error stop "FAIL: 2D size"
    if (c44_total_size(i3) /= 12) error stop "FAIL: 3D size"
    if (c44_total_size(r1) /= 4)  error stop "FAIL: real 1D size"
    if (c44_total_size(dt1) /= 3) error stop "FAIL: struct 1D size"

    ! ---- Data access and type-based dispatch ----
    if (c44_sum_star(is) /= 42)  error stop "FAIL: int scalar sum"
    if (c44_sum_star(i1) /= 15)  error stop "FAIL: int 1D sum"
    if (c44_sum_star(i2) /= 78)  error stop "FAIL: int 2D sum"
    if (c44_sum_star(i3) /= 186) error stop "FAIL: int 3D sum"
    if (c44_sum_star(r1) /= 10)  error stop "FAIL: real 1D sum"

    ! ---- OPTIONAL present ----
    if (c44_opt_rank(i1) /= 1) error stop "FAIL: opt present"

    ! ---- OPTIONAL absent ----
    if (c44_opt_rank() /= -1) error stop "FAIL: opt absent"

    print *, "All bindc_44 tests passed."
end program
