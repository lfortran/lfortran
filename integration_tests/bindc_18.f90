module bindc_18_mod
    use iso_c_binding, only: c_int, c_int32_t, c_int64_t, c_float, c_double
    implicit none

    interface
        ! ---- int32 sum: ranks 1, 2, 3 and assumed rank ----
        integer(c_int32_t) function c_sum_int32_1d(a) bind(C, name="c_sum_int32_1d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function
        integer(c_int32_t) function c_sum_int32_2d(a) bind(C, name="c_sum_int32_2d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:)
        end function
        integer(c_int32_t) function c_sum_int32_3d(a) bind(C, name="c_sum_int32_3d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:,:)
        end function
        integer(c_int32_t) function c_sum_int32_ar(a) bind(C, name="c_sum_int32_ar")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(..)
        end function

        ! ---- int64 sum: ranks 1, 2, 3 ----
        integer(c_int64_t) function c_sum_int64_1d(a) bind(C, name="c_sum_int64_1d")
            import :: c_int64_t
            integer(c_int64_t), intent(in) :: a(:)
        end function
        integer(c_int64_t) function c_sum_int64_2d(a) bind(C, name="c_sum_int64_2d")
            import :: c_int64_t
            integer(c_int64_t), intent(in) :: a(:,:)
        end function
        integer(c_int64_t) function c_sum_int64_3d(a) bind(C, name="c_sum_int64_3d")
            import :: c_int64_t
            integer(c_int64_t), intent(in) :: a(:,:,:)
        end function

        ! ---- float sum: ranks 1, 2, 3 ----
        real(c_float) function c_sum_float_1d(a) bind(C, name="c_sum_float_1d")
            import :: c_float
            real(c_float), intent(in) :: a(:)
        end function
        real(c_float) function c_sum_float_2d(a) bind(C, name="c_sum_float_2d")
            import :: c_float
            real(c_float), intent(in) :: a(:,:)
        end function
        real(c_float) function c_sum_float_3d(a) bind(C, name="c_sum_float_3d")
            import :: c_float
            real(c_float), intent(in) :: a(:,:,:)
        end function

        ! ---- double sum: ranks 1, 2, 3 ----
        real(c_double) function c_sum_double_1d(a) bind(C, name="c_sum_double_1d")
            import :: c_double
            real(c_double), intent(in) :: a(:)
        end function
        real(c_double) function c_sum_double_2d(a) bind(C, name="c_sum_double_2d")
            import :: c_double
            real(c_double), intent(in) :: a(:,:)
        end function
        real(c_double) function c_sum_double_3d(a) bind(C, name="c_sum_double_3d")
            import :: c_double
            real(c_double), intent(in) :: a(:,:,:)
        end function

        ! ---- int32 double-in-place: ranks 1, 2, 3 ----
        subroutine c_scale_int32_1d(a) bind(C, name="c_double_int32_1d")
            import :: c_int32_t
            integer(c_int32_t), intent(inout) :: a(:)
        end subroutine
        subroutine c_scale_int32_2d(a) bind(C, name="c_double_int32_2d")
            import :: c_int32_t
            integer(c_int32_t), intent(inout) :: a(:,:)
        end subroutine
        subroutine c_scale_int32_3d(a) bind(C, name="c_double_int32_3d")
            import :: c_int32_t
            integer(c_int32_t), intent(inout) :: a(:,:,:)
        end subroutine

        ! ---- assumed-rank queries: type(*), dimension(..) ----
        integer(c_int) function c_get_rank(a) bind(C, name="c_get_rank")
            import :: c_int
            type(*), intent(in) :: a(..)
        end function

        integer(c_int) function c_get_elem_size(a) bind(C, name="c_get_elem_size")
            import :: c_int
            type(*), intent(in) :: a(..)
        end function
    end interface
end module

program bindc_18
    use bindc_18_mod
    use iso_c_binding, only: c_int32_t, c_int64_t, c_float, c_double
    implicit none

    call test_sum_int32()
    call test_sum_int64()
    call test_sum_float()
    call test_sum_double()
    call test_double_inplace()
    call test_assumed_rank()

    print *, "All bindc_18 tests passed."

contains

    subroutine test_sum_int32()
        integer(c_int32_t) :: a1(4), a2(2,3), a3(2,3,2)
        integer :: i

        a1 = [1, 2, 3, 4]
        a2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        a3 = reshape([(i, i=1,12)], [2, 3, 2])

        if (c_sum_int32_1d(a1) /= 10) error stop "FAIL: int32 1d sum"
        if (c_sum_int32_2d(a2) /= 21) error stop "FAIL: int32 2d sum"
        if (c_sum_int32_3d(a3) /= 78) error stop "FAIL: int32 3d sum"
    end subroutine

    subroutine test_sum_int64()
        integer(c_int64_t) :: a1(4), a2(2,3), a3(2,3,2)
        integer :: i

        a1 = [1_c_int64_t, 2_c_int64_t, 3_c_int64_t, 4_c_int64_t]
        a2 = reshape([1_c_int64_t, 2_c_int64_t, 3_c_int64_t, &
                      4_c_int64_t, 5_c_int64_t, 6_c_int64_t], [2, 3])
        a3 = reshape([(int(i, c_int64_t), i=1,12)], [2, 3, 2])

        if (c_sum_int64_1d(a1) /= 10_c_int64_t) error stop "FAIL: int64 1d sum"
        if (c_sum_int64_2d(a2) /= 21_c_int64_t) error stop "FAIL: int64 2d sum"
        if (c_sum_int64_3d(a3) /= 78_c_int64_t) error stop "FAIL: int64 3d sum"
    end subroutine

    subroutine test_sum_float()
        real(c_float) :: a1(4), a2(2,3), a3(2,3,2)
        integer :: i

        a1 = [1.0, 2.0, 3.0, 4.0]
        a2 = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2, 3])
        a3 = reshape([(real(i), i=1,12)], [2, 3, 2])

        if (abs(c_sum_float_1d(a1) - 10.0) > 1.0e-5) error stop "FAIL: float 1d sum"
        if (abs(c_sum_float_2d(a2) - 21.0) > 1.0e-5) error stop "FAIL: float 2d sum"
        if (abs(c_sum_float_3d(a3) - 78.0) > 1.0e-5) error stop "FAIL: float 3d sum"
    end subroutine

    subroutine test_sum_double()
        real(c_double) :: a1(4), a2(2,3), a3(2,3,2)
        integer :: i

        a1 = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
        a2 = reshape([1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0], [2, 3])
        a3 = reshape([(dble(i), i=1,12)], [2, 3, 2])

        if (abs(c_sum_double_1d(a1) - 10.0d0) > 1.0d-10) error stop "FAIL: double 1d sum"
        if (abs(c_sum_double_2d(a2) - 21.0d0) > 1.0d-10) error stop "FAIL: double 2d sum"
        if (abs(c_sum_double_3d(a3) - 78.0d0) > 1.0d-10) error stop "FAIL: double 3d sum"
    end subroutine

    subroutine test_double_inplace()
        integer(c_int32_t) :: a1(4), a2(2,3), a3(2,3,2)
        integer :: i

        ! rank 1
        a1 = [1, 2, 3, 4]
        call c_scale_int32_1d(a1)
        if (a1(1) /= 2 .or. a1(4) /= 8) error stop "FAIL: scale int32 1d"
        if (c_sum_int32_1d(a1) /= 20) error stop "FAIL: scale int32 1d sum"

        ! rank 2
        a2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call c_scale_int32_2d(a2)
        if (a2(1,1) /= 2 .or. a2(2,3) /= 12) error stop "FAIL: scale int32 2d"
        if (c_sum_int32_2d(a2) /= 42) error stop "FAIL: scale int32 2d sum"

        ! rank 3
        a3 = reshape([(i, i=1,12)], [2, 3, 2])
        call c_scale_int32_3d(a3)
        if (a3(1,1,1) /= 2 .or. a3(2,3,2) /= 24) error stop "FAIL: scale int32 3d"
        if (c_sum_int32_3d(a3) /= 156) error stop "FAIL: scale int32 3d sum"
    end subroutine

    subroutine test_assumed_rank()
        integer(c_int32_t) :: ai1(4), ai2(2,3), ai3(2,3,2)
        real(c_double)     :: ad1(3)
        integer(c_int64_t) :: al1(2)
        integer :: i

        ai1 = [1, 2, 3, 4]
        ai2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        ai3 = reshape([(i, i=1,12)], [2, 3, 2])
        ad1 = [1.0d0, 2.0d0, 3.0d0]
        al1 = [10_c_int64_t, 20_c_int64_t]

        ! rank query
        if (c_get_rank(ai1) /= 1) error stop "FAIL: rank of ai1"
        if (c_get_rank(ai2) /= 2) error stop "FAIL: rank of ai2"
        if (c_get_rank(ai3) /= 3) error stop "FAIL: rank of ai3"
        if (c_get_rank(ad1) /= 1) error stop "FAIL: rank of ad1"
        if (c_get_rank(al1) /= 1) error stop "FAIL: rank of al1"

        ! element size query
        if (c_get_elem_size(ai1) /= 4) error stop "FAIL: elem_size int32"
        if (c_get_elem_size(ad1) /= 8) error stop "FAIL: elem_size double"
        if (c_get_elem_size(al1) /= 8) error stop "FAIL: elem_size int64"

        ! assumed-rank sum (int32 passed as rank 1, 2, 3)
        if (c_sum_int32_ar(ai1) /= 10) error stop "FAIL: assumed-rank int32 1d"
        if (c_sum_int32_ar(ai2) /= 21) error stop "FAIL: assumed-rank int32 2d"
        if (c_sum_int32_ar(ai3) /= 78) error stop "FAIL: assumed-rank int32 3d"
    end subroutine

end program
