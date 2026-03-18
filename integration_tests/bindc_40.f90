! Test: CFI_type_cptr/cfunptr, internal BIND(C),
!       deferred-length character via descriptor, explicit-shape multi-dim
!
! Covers:
!   - CFI_type_cptr type code through descriptor
!   - CFI_type_cfunptr type code through descriptor (flang bug: missing macro)
!   - BIND(C) on internal procedure (F2018)
!   - Deferred-length allocatable character through descriptor
!   - Multi-dimensional explicit-shape arrays in BIND(C)

module bindc_40_ifaces
    use iso_c_binding
    implicit none

    interface
        ! ---- CFI_type_cptr: pass c_ptr array through descriptor ----
        integer(c_int) function c40_check_cptr_type(arr) bind(C)
            import :: c_int, c_ptr
            type(c_ptr), intent(in) :: arr(:)
        end function

        ! ---- CFI_type_cfunptr: pass c_funptr array through descriptor ----
        integer(c_int) function c40_check_cfunptr_type(arr) bind(C)
            import :: c_int, c_funptr
            type(c_funptr), intent(in) :: arr(:)
        end function

        ! ---- Deferred-length allocatable character ----
        integer(c_int) function c40_check_deferred_char(s) bind(C)
            import :: c_int, c_char
            character(kind=c_char, len=:), allocatable, intent(in) :: s
        end function

        ! ---- Explicit-shape 2D array ----
        integer(c_int) function c40_sum_explicit_2d(arr, r, c) bind(C)
            import :: c_int
            integer(c_int), value :: r, c
            integer(c_int), intent(in) :: arr(r, c)
        end function

        ! ---- Explicit-shape 3D array ----
        integer(c_int) function c40_sum_explicit_3d(arr, d1, d2, d3) bind(C)
            import :: c_int
            integer(c_int), value :: d1, d2, d3
            integer(c_int), intent(in) :: arr(d1, d2, d3)
        end function

        ! ---- Internal BIND(C) proc: C calls this ----
        integer(c_int) function c40_call_internal(fp, x) bind(C)
            import :: c_int, c_funptr
            type(c_funptr), value :: fp
            integer(c_int), value :: x
        end function
    end interface
end module

program bindc_40
    use iso_c_binding
    use bindc_40_ifaces
    implicit none

    integer :: i

    call test_cptr_type_code()
    call test_cfunptr_type_code()
    call test_deferred_char()
    call test_explicit_shape_multidim()
    call test_internal_bindc()

    print *, "All bindc_40 tests passed."

contains

    subroutine test_cptr_type_code()
        type(c_ptr) :: ptrs(3)
        integer(c_int), target :: vals(3)
        integer :: j
        vals = [10, 20, 30]
        do j = 1, 3
            ptrs(j) = c_loc(vals(j))
        end do
        ! C checks desc->type == CFI_type_cptr
        if (c40_check_cptr_type(ptrs) /= 1) error stop "FAIL: CFI_type_cptr"
    end subroutine

    subroutine test_cfunptr_type_code()
        type(c_funptr) :: fps(2)
        fps(1) = c_funloc(internal_add)
        fps(2) = c_funloc(internal_add)
        ! C checks desc->type == CFI_type_cfunptr
        ! Note: flang is missing CFI_type_cfunptr macro (flang bug)
        if (c40_check_cfunptr_type(fps) /= 1) error stop "FAIL: CFI_type_cfunptr"
    end subroutine

    subroutine test_deferred_char()
        character(kind=c_char, len=:), allocatable :: s
        integer(c_int) :: r
        s = "Hello"
        ! C checks elem_len == 5 and attribute == allocatable
        r = c40_check_deferred_char(s)
        if (r /= 5) error stop "FAIL: deferred char elem_len"
    end subroutine

    subroutine test_explicit_shape_multidim()
        integer(c_int) :: a2d(3, 2)
        integer(c_int) :: a3d(2, 3, 2)
        integer(c_int) :: r

        a2d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        r = c40_sum_explicit_2d(a2d, 3_c_int, 2_c_int)
        if (r /= 21) error stop "FAIL: explicit 2D sum"

        a3d = reshape([(i, i = 1, 12)], [2, 3, 2])
        r = c40_sum_explicit_3d(a3d, 2_c_int, 3_c_int, 2_c_int)
        if (r /= 78) error stop "FAIL: explicit 3D sum"
    end subroutine

    subroutine test_internal_bindc()
        type(c_funptr) :: fp
        integer(c_int) :: r
        fp = c_funloc(internal_add)
        r = c40_call_internal(fp, 7_c_int)
        if (r /= 107) error stop "FAIL: internal bindc proc"
    end subroutine

    ! Internal BIND(C) procedure (F2018 allows this)
    integer(c_int) function internal_add(x) bind(C)
        integer(c_int), value :: x
        internal_add = x + 100
    end function

end program
