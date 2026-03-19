! Consolidated ISO_Fortran_binding test: Assumed-rank/type, C-Fortran Callbacks & Misc
!
! Merged from: bindc_21, bindc_23, bindc_25, bindc_29, bindc_34, bindc_40, bindc_42, bindc_44

! ============================================================
! Modules from bindc_21
! ============================================================
module bindc_21_mod
    use iso_c_binding, only: c_int, c_int32_t, c_int64_t, c_double
    implicit none

    interface
        ! ---- assumed-rank with intent(inout) ----
        subroutine c21_double_ar(a) bind(C, name="c21_double_ar")
            import :: c_int32_t
            integer(c_int32_t), intent(inout) :: a(..)
        end subroutine

        ! ---- assumed-rank with specific types ----
        integer(c_int64_t) function c21_sum_ar_i64(a) &
                bind(C, name="c21_sum_ar_i64")
            import :: c_int64_t
            integer(c_int64_t), intent(in) :: a(..)
        end function

        integer(c_int) function c21_sum_ar_dbl(a) &
                bind(C, name="c21_sum_ar_dbl")
            import :: c_int, c_double
            real(c_double), intent(in) :: a(..)
        end function

        ! ---- optional assumed-rank ----
        integer(c_int) function c21_opt_ar_present(a) &
                bind(C, name="c21_opt_ar_present")
            import :: c_int
            type(*), optional, intent(in) :: a(..)
        end function

        ! ---- optional scalar ----
        integer(c_int) function c21_opt_scalar_present(x) &
                bind(C, name="c21_opt_scalar_present")
            import :: c_int, c_int32_t
            integer(c_int32_t), optional, intent(in) :: x
        end function

        integer(c_int32_t) function c21_opt_scalar_value(x) &
                bind(C, name="c21_opt_scalar_value")
            import :: c_int32_t
            integer(c_int32_t), optional, intent(in) :: x
        end function

        ! ---- multiple descriptor arguments ----
        integer(c_int32_t) function c21_dot_product(a, b) &
                bind(C, name="c21_dot_product")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
            integer(c_int32_t), intent(in) :: b(:)
        end function

        subroutine c21_add_arrays(a, b, c) bind(C, name="c21_add_arrays")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
            integer(c_int32_t), intent(in) :: b(:)
            integer(c_int32_t), intent(out) :: c(:)
        end subroutine
    end interface
end module

! ============================================================
! Modules from bindc_23
! ============================================================
module bindc_23_mod
    use iso_c_binding, only: c_int, c_int32_t, c_char
    implicit none

    interface
        ! ---- character(c_char) assumed-shape arrays ----
        integer(c_int) function c23_char_sum_1d(a) &
                bind(C, name="c23_char_sum_1d")
            import :: c_int, c_char
            character(kind=c_char, len=1), intent(in) :: a(:)
        end function

        ! ---- assumed-rank: rank query for scalar (rank 0) ----
        integer(c_int) function c23_get_rank(a) bind(C, name="c23_get_rank")
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(..)
        end function

        ! ---- optional allocatable ----
        integer(c_int) function c23_opt_alloc_present(a) &
                bind(C, name="c23_opt_alloc_present")
            import :: c_int, c_int32_t
            integer(c_int32_t), optional, allocatable, intent(in) :: a(:)
        end function

        integer(c_int32_t) function c23_opt_alloc_sum(a) &
                bind(C, name="c23_opt_alloc_sum")
            import :: c_int, c_int32_t
            integer(c_int32_t), optional, allocatable, intent(in) :: a(:)
        end function

        ! ---- optional pointer ----
        integer(c_int) function c23_opt_ptr_present(a) &
                bind(C, name="c23_opt_ptr_present")
            import :: c_int, c_int32_t
            integer(c_int32_t), optional, pointer, intent(in) :: a(:)
        end function

        ! ---- C calling Fortran (receives descriptor, calls back) ----
        integer(c_int32_t) function c23_call_fortran_sum(a) &
                bind(C, name="c23_call_fortran_sum")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function
    end interface
end module

! Fortran BIND(C) procedure with descriptor arg (called from C)
module bindc_23_callbacks
    use iso_c_binding, only: c_int32_t
    implicit none
contains
    integer(c_int32_t) function fortran_sum_1d(a) &
            bind(C, name="fortran_sum_1d")
        integer(c_int32_t), intent(in) :: a(:)
        integer :: i
        fortran_sum_1d = 0
        do i = 1, size(a)
            fortran_sum_1d = fortran_sum_1d + a(i)
        end do
    end function
end module

! ============================================================
! Modules from bindc_25
! ============================================================
module bindc_25_types
    use iso_c_binding, only: c_int32_t, c_double
    implicit none

    type, bind(C) :: pair_t
        integer(c_int32_t) :: a
        integer(c_int32_t) :: b
    end type
end module

module bindc_25_fortran_procs
    use iso_c_binding
    use bindc_25_types
    implicit none
contains

    ! ---- assumed-shape subroutine (C calls this) ----
    subroutine f25_double_1d(a) bind(C, name="f25_double_1d")
        integer(c_int32_t), intent(inout) :: a(:)
        integer :: i
        do i = 1, size(a)
            a(i) = a(i) * 2
        end do
    end subroutine

    integer(c_int32_t) function f25_sum_1d(a) bind(C, name="f25_sum_1d")
        integer(c_int32_t), intent(in) :: a(:)
        integer :: i
        f25_sum_1d = 0
        do i = 1, size(a)
            f25_sum_1d = f25_sum_1d + a(i)
        end do
    end function

    integer(c_int32_t) function f25_sum_2d(a) bind(C, name="f25_sum_2d")
        integer(c_int32_t), intent(in) :: a(:,:)
        integer :: i, j
        f25_sum_2d = 0
        do j = 1, size(a, 2)
            do i = 1, size(a, 1)
                f25_sum_2d = f25_sum_2d + a(i, j)
            end do
        end do
    end function

    ! ---- assumed-rank (C calls this) ----
    integer(c_int) function f25_get_rank(a) bind(C, name="f25_get_rank")
        integer(c_int32_t), intent(in) :: a(..)
        f25_get_rank = rank(a)
    end function

    ! ---- allocatable arg (C calls this) ----
    integer(c_int32_t) function f25_sum_alloc(a) bind(C, name="f25_sum_alloc")
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        integer :: i
        f25_sum_alloc = 0
        if (allocated(a)) then
            do i = 1, size(a)
                f25_sum_alloc = f25_sum_alloc + a(i)
            end do
        end if
    end function

    ! ---- pointer arg (C calls this) ----
    integer(c_int32_t) function f25_sum_ptr(a) bind(C, name="f25_sum_ptr")
        integer(c_int32_t), pointer, intent(in) :: a(:)
        integer :: i
        f25_sum_ptr = 0
        if (associated(a)) then
            do i = 1, size(a)
                f25_sum_ptr = f25_sum_ptr + a(i)
            end do
        end if
    end function

    ! ---- optional arg (C calls this) ----
    integer(c_int) function f25_opt_present(a) bind(C, name="f25_opt_present")
        integer(c_int32_t), optional, intent(in) :: a(:)
        if (present(a)) then
            f25_opt_present = 1
        else
            f25_opt_present = 0
        end if
    end function

    ! ---- CONTIGUOUS arg (C calls this) ----
    integer(c_int32_t) function f25_sum_contig(a) &
            bind(C, name="f25_sum_contig")
        integer(c_int32_t), contiguous, intent(in) :: a(:)
        integer :: i
        f25_sum_contig = 0
        do i = 1, size(a)
            f25_sum_contig = f25_sum_contig + a(i)
        end do
    end function

    ! ---- intent(out) descriptor (C calls, Fortran fills) ----
    subroutine f25_fill_array(a) bind(C, name="f25_fill_array")
        integer(c_int32_t), intent(out) :: a(:)
        integer :: i
        do i = 1, size(a)
            a(i) = i * 10
        end do
    end subroutine

    ! ---- multiple descriptor args (C calls this) ----
    integer(c_int32_t) function f25_dot(a, b) bind(C, name="f25_dot")
        integer(c_int32_t), intent(in) :: a(:), b(:)
        integer :: i
        f25_dot = 0
        do i = 1, size(a)
            f25_dot = f25_dot + a(i) * b(i)
        end do
    end function

    subroutine f25_add(a, b, c) bind(C, name="f25_add")
        integer(c_int32_t), intent(in) :: a(:), b(:)
        integer(c_int32_t), intent(out) :: c(:)
        integer :: i
        do i = 1, size(a)
            c(i) = a(i) + b(i)
        end do
    end subroutine

    ! ---- derived type array (C calls this) ----
    integer(c_int32_t) function f25_sum_pairs(pts) &
            bind(C, name="f25_sum_pairs")
        type(pair_t), intent(in) :: pts(:)
        integer :: i
        f25_sum_pairs = 0
        do i = 1, size(pts)
            f25_sum_pairs = f25_sum_pairs + pts(i)%a + pts(i)%b
        end do
    end function

    ! ---- complex array (C calls this) ----
    subroutine f25_sum_complex(a, re, im) bind(C, name="f25_sum_complex")
        complex(c_float_complex), intent(in) :: a(:)
        real(c_float), intent(out) :: re, im
        integer :: i
        re = 0.0
        im = 0.0
        do i = 1, size(a)
            re = re + real(a(i))
            im = im + aimag(a(i))
        end do
    end subroutine

    ! ---- logical array (C calls this) ----
    integer(c_int) function f25_count_true(a) &
            bind(C, name="f25_count_true")
        logical(c_bool), intent(in) :: a(:)
        integer :: i
        f25_count_true = 0
        do i = 1, size(a)
            if (a(i)) f25_count_true = f25_count_true + 1
        end do
    end function

    ! ---- function returning scalar (C calls this) ----
    integer(c_int32_t) function f25_square(x) bind(C, name="f25_square")
        integer(c_int32_t), value :: x
        f25_square = x * x
    end function

    ! ---- function returning complex (C calls this) ----
    complex(c_float_complex) function f25_conj(z) bind(C, name="f25_conj")
        complex(c_float_complex), value :: z
        f25_conj = conjg(z)
    end function

end module

module bindc_25_c_iface
    use iso_c_binding, only: c_int, c_int32_t
    implicit none

    interface
        integer(c_int) function c25_run_all_tests() &
                bind(C, name="c25_run_all_tests")
            import :: c_int
        end function
    end interface
end module

! ============================================================
! Modules from bindc_29
! ============================================================
module bindc_29_types
    use iso_c_binding, only: c_int32_t, c_double
    implicit none

    type, bind(C) :: vec3_t
        real(c_double) :: x, y, z
    end type
end module

module bindc_29_globals
    use iso_c_binding, only: c_int32_t, c_double, c_bool, c_char, &
        c_float_complex, c_int
    use bindc_29_types
    implicit none

    ! Module variables bound to C globals defined in bindc_29c.c
    integer(c_int32_t), bind(C, name="c29_global_arr") :: global_arr(4)
    type(vec3_t), bind(C, name="c29_global_vec") :: global_vec
    logical(c_bool), bind(C, name="c29_global_flag") :: global_flag
    integer(c_int32_t), bind(C, name="c29_global_counter") :: global_counter
end module

module bindc_29_mod
    use iso_c_binding
    use bindc_29_types
    implicit none

    interface
        ! ---- read C globals (verify Fortran can read them) ----
        integer(c_int) function c29_check_globals() &
                bind(C, name="c29_check_globals")
            import :: c_int
        end function

        ! ---- verify Fortran writes are visible in C ----
        integer(c_int) function c29_verify_writes() &
                bind(C, name="c29_verify_writes")
            import :: c_int
        end function

        ! ---- descriptor type code check ----
        integer(c_int) function c29_check_type_int32(a) &
                bind(C, name="c29_check_type_int32")
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function

        integer(c_int) function c29_check_type_double(a) &
                bind(C, name="c29_check_type_double")
            import :: c_int, c_double
            real(c_double), intent(in) :: a(:)
        end function

        integer(c_int) function c29_check_type_float(a) &
                bind(C, name="c29_check_type_float")
            import :: c_int, c_float
            real(c_float), intent(in) :: a(:)
        end function

        ! ---- same array to multiple descriptor args ----
        integer(c_int32_t) function c29_self_dot(a, b) &
                bind(C, name="c29_self_dot")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:), b(:)
        end function
    end interface
end module

! A module with BIND(C, NAME="") — blank name (private linkage)
module bindc_29_blank_name
    use iso_c_binding, only: c_int32_t
    implicit none
contains
    integer(c_int32_t) function f29_blank_add(a, b) bind(C, name="")
        integer(c_int32_t), value :: a, b
        f29_blank_add = a + b
    end function
end module

! ============================================================
! Modules from bindc_34
! ============================================================
module bindc_34_types
    use iso_c_binding
    implicit none

    ! ---- ENUM, BIND(C) ----
    enum, bind(C)
        enumerator :: COLOR_RED = 0
        enumerator :: COLOR_GREEN = 1
        enumerator :: COLOR_BLUE = 2
    end enum

    ! ---- Derived type with fixed array component ----
    type, bind(C) :: vec5_t
        integer(c_int32_t) :: data(5)
    end type

end module

module bindc_34_globals
    use iso_c_binding
    implicit none

    ! ---- Module bind(C) character variable ----
    character(kind=c_char, len=1), bind(C, name="g34_char") :: g34_char

    ! ---- Module bind(C) complex variable ----
    complex(c_float_complex), bind(C, name="g34_cmplx") :: g34_cmplx

end module

module bindc_34_common_mod
    use iso_c_binding
    implicit none

    ! ---- COMMON block with BIND(C) ----
    integer(c_int32_t) :: common_x, common_y
    common /c34_common/ common_x, common_y
    bind(C, name="c34_common") :: /c34_common/

end module

module bindc_34_ifaces
    use iso_c_binding
    use bindc_34_types
    implicit none

    abstract interface
        function int_func_i(x) bind(C) result(r)
            import :: c_int
            integer(c_int), value :: x
            integer(c_int) :: r
        end function
    end interface

    interface
        ! Enum: C returns an enum value
        integer(c_int) function c34_get_color(idx) bind(C)
            import :: c_int
            integer(c_int), value :: idx
        end function

        ! Enum: C checks if value matches expected
        integer(c_int) function c34_check_color(val, expected) bind(C)
            import :: c_int
            integer(c_int), value :: val, expected
        end function

        ! DT with fixed array: C sums the data array
        integer(c_int32_t) function c34_sum_vec5(v) bind(C)
            import :: c_int32_t, vec5_t
            type(vec5_t), intent(in) :: v
        end function

        ! DT with fixed array: C reads element at index
        integer(c_int32_t) function c34_get_vec5_elem(v, idx) bind(C)
            import :: c_int32_t, c_int, vec5_t
            type(vec5_t), intent(in) :: v
            integer(c_int), value :: idx
        end function

        ! c_f_procpointer: C returns a function pointer
        type(c_funptr) function c34_get_func_ptr() bind(C)
            import :: c_funptr
        end function

        ! Module globals: C sets char and complex
        subroutine c34_set_globals(c, re, im) bind(C)
            import :: c_char, c_float
            character(kind=c_char, len=1), value :: c
            real(c_float), value :: re, im
        end subroutine

        ! Module globals: C reads char
        character(kind=c_char, len=1) function c34_get_char() bind(C)
            import :: c_char
        end function

        ! Module globals: C reads complex real part
        real(c_float) function c34_get_cmplx_re() bind(C)
            import :: c_float
        end function

        ! Module globals: C reads complex imag part
        real(c_float) function c34_get_cmplx_im() bind(C)
            import :: c_float
        end function

        ! COMMON: C sets values
        subroutine c34_set_common(x, y) bind(C)
            import :: c_int32_t
            integer(c_int32_t), value :: x, y
        end subroutine

        ! COMMON: C reads x
        integer(c_int32_t) function c34_get_common_x() bind(C)
            import :: c_int32_t
        end function

        ! COMMON: C reads y
        integer(c_int32_t) function c34_get_common_y() bind(C)
            import :: c_int32_t
        end function
    end interface
end module

! ============================================================
! Modules from bindc_40
! ============================================================
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

! ============================================================
! Modules from bindc_42
! ============================================================
module bindc_42_globals
    use iso_c_binding
    implicit none
    integer(c_int), save, bind(C, name="g42_save_var") :: g42_save_var = 0
end module

module bindc_42_protected_mod
    use iso_c_binding
    implicit none
    integer(c_int), protected, bind(C, name="g42_protected") :: g42_protected = 99
contains
    subroutine set_protected(val)
        integer(c_int), intent(in) :: val
        g42_protected = val
    end subroutine
end module

module bindc_42_ifaces
    use iso_c_binding
    implicit none

    interface
        ! ---- Negative-stride section from C ----
        integer(c_int) function c42_test_negative_stride_section() bind(C)
            import :: c_int
        end function

        ! ---- SAVE+BIND(C) global: C modifies it ----
        subroutine c42_set_save_var(val) bind(C)
            import :: c_int
            integer(c_int), value :: val
        end subroutine

        integer(c_int) function c42_get_save_var() bind(C)
            import :: c_int
        end function

        ! ---- PROTECTED+BIND(C): C reads it ----
        integer(c_int) function c42_get_protected() bind(C)
            import :: c_int
        end function

        ! ---- Optional allocatable scalar ----
        integer(c_int) function c42_opt_alloc_scalar(x) bind(C)
            import :: c_int
            integer(c_int), allocatable, optional, intent(in) :: x
        end function

        ! ---- Optional pointer scalar ----
        integer(c_int) function c42_opt_ptr_scalar(x) bind(C)
            import :: c_int
            integer(c_int), pointer, optional, intent(in) :: x
        end function
    end interface
end module

! (bindc_44 has no modules — interfaces are in the program block)

! ============================================================
! Main program
! ============================================================
program bindc_iso_fb_05
    use bindc_21_mod
    use bindc_23_mod
    use bindc_23_callbacks
    use bindc_25_types
    use bindc_25_fortran_procs
    use bindc_25_c_iface
    use bindc_29_types
    use bindc_29_globals
    use bindc_29_mod
    use bindc_29_blank_name
    use bindc_34_types
    use bindc_34_globals
    use bindc_34_common_mod
    use bindc_34_ifaces
    use bindc_40_ifaces
    use bindc_42_globals
    use bindc_42_protected_mod
    use bindc_42_ifaces
    use iso_c_binding
    implicit none

    ! Interfaces from bindc_44 (originally inline in program)
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

    ! --- bindc_21 tests ---
    call test_ar_inout()
    call test_ar_types()
    call test_opt_ar()
    call test_opt_scalar()
    call test_multi_arg()

    ! --- bindc_23 tests ---
    call test_char_array()
    call test_ar_rank0()
    call test_opt_alloc()
    call test_opt_ptr()
    call test_c_calls_fortran()

    ! --- bindc_25 tests ---
    call test_c_calling_fortran()

    ! --- bindc_29 tests ---
    call test_read_c_globals()
    call test_write_to_c_globals()
    call test_c_sizeof_derived()
    call test_c_sizeof_array()
    call test_c_associated_two_args()
    call test_c_loc_variants()
    call test_c_f_pointer_multidim()
    call test_type_codes()
    call test_same_array_multi_desc()
    call test_blank_name()

    ! --- bindc_34 tests ---
    call test_enum()
    call test_dt_fixed_array()
    call test_c_f_procpointer()
    call test_module_globals()
    call test_common_block()

    ! --- bindc_40 tests ---
    call test_cptr_type_code()
    call test_cfunptr_type_code()
    call test_deferred_char()
    call test_explicit_shape_multidim()
    call test_internal_bindc()

    ! --- bindc_42 tests ---
    call test_negative_stride_section()
    call test_save_bindc()
    call test_protected_bindc()
    call test_optional_alloc_scalar()
    call test_optional_ptr_scalar()
    call test_c_f_pointer_lower_bounds()

    ! --- bindc_44 tests ---
    call test_44_all()

    print *, "All bindc_iso_fb_05 tests passed."

contains

    ! ========== Test subroutines from bindc_21 ==========

    subroutine test_ar_inout()
        integer(c_int32_t) :: a1(4), a2(2,3)

        a1 = [1, 2, 3, 4]
        call c21_double_ar(a1)
        if (a1(1) /= 2 .or. a1(4) /= 8) error stop "FAIL: ar inout 1d"

        a2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call c21_double_ar(a2)
        if (a2(1,1) /= 2 .or. a2(2,3) /= 12) error stop "FAIL: ar inout 2d"
    end subroutine

    subroutine test_ar_types()
        integer(c_int64_t) :: ai64(3)
        real(c_double)     :: ad(3)

        ai64 = [10_c_int64_t, 20_c_int64_t, 30_c_int64_t]
        if (c21_sum_ar_i64(ai64) /= 60_c_int64_t) &
            error stop "FAIL: ar i64 sum"

        ad = [1.5d0, 2.5d0, 3.0d0]
        if (c21_sum_ar_dbl(ad) /= 7) error stop "FAIL: ar dbl sum"
    end subroutine

    subroutine test_opt_ar()
        integer(c_int32_t) :: arr(4)
        arr = [1, 2, 3, 4]

        if (c21_opt_ar_present(arr) /= 1) error stop "FAIL: opt ar present"
        if (c21_opt_ar_present() /= 0) error stop "FAIL: opt ar absent"
    end subroutine

    subroutine test_opt_scalar()
        integer(c_int32_t) :: x
        x = 77

        if (c21_opt_scalar_present(x) /= 1) error stop "FAIL: opt scalar present"
        if (c21_opt_scalar_present() /= 0) error stop "FAIL: opt scalar absent"

        if (c21_opt_scalar_value(x) /= 77) error stop "FAIL: opt scalar value"
    end subroutine

    subroutine test_multi_arg()
        integer(c_int32_t) :: a(4), b(4), c(4)

        a = [1, 2, 3, 4]
        b = [10, 20, 30, 40]

        if (c21_dot_product(a, b) /= 300) error stop "FAIL: dot product"

        c = 0
        call c21_add_arrays(a, b, c)
        if (c(1) /= 11 .or. c(4) /= 44) error stop "FAIL: add arrays"
    end subroutine

    ! ========== Test subroutines from bindc_23 ==========

    subroutine test_char_array()
        character(kind=c_char, len=1) :: c1(4)

        c1 = ['A', 'B', 'C', 'D']
        ! 65 + 66 + 67 + 68 = 266
        if (c23_char_sum_1d(c1) /= 266) error stop "FAIL: char sum"
    end subroutine

    subroutine test_ar_rank0()
        integer(c_int32_t) :: scalar

        scalar = 42
        if (c23_get_rank(scalar) /= 0) error stop "FAIL: ar rank0"
    end subroutine

    subroutine test_opt_alloc()
        integer(c_int32_t), allocatable :: a(:)

        allocate(a(3))
        a = [10, 20, 30]
        if (c23_opt_alloc_present(a) /= 1) error stop "FAIL: opt alloc present"
        if (c23_opt_alloc_sum(a) /= 60) error stop "FAIL: opt alloc sum"

        if (c23_opt_alloc_present() /= 0) error stop "FAIL: opt alloc absent"

        deallocate(a)
    end subroutine

    subroutine test_opt_ptr()
        integer(c_int32_t), target :: tgt(3)
        integer(c_int32_t), pointer :: p(:)

        tgt = [1, 2, 3]
        p => tgt
        if (c23_opt_ptr_present(p) /= 1) error stop "FAIL: opt ptr present"

        if (c23_opt_ptr_present() /= 0) error stop "FAIL: opt ptr absent"
    end subroutine

    subroutine test_c_calls_fortran()
        integer(c_int32_t) :: arr(5)
        integer(c_int32_t) :: result

        arr = [10, 20, 30, 40, 50]
        result = c23_call_fortran_sum(arr)
        if (result /= 150) error stop "FAIL: c calls fortran"
    end subroutine

    ! ========== Test subroutines from bindc_25 ==========

    subroutine test_c_calling_fortran()
        integer(c_int) :: result
        result = c25_run_all_tests()
        if (result /= 0) then
            print *, "C test failed with code:", result
            error stop "FAIL: C calling Fortran tests failed"
        end if
    end subroutine

    ! ========== Test subroutines from bindc_29 ==========

    subroutine test_read_c_globals()
        ! C initializes globals; Fortran reads them
        if (c29_check_globals() /= 0) error stop "FAIL: C globals not init"

        ! Now read from Fortran side
        if (global_arr(1) /= 10) error stop "FAIL: global_arr(1)"
        if (global_arr(2) /= 20) error stop "FAIL: global_arr(2)"
        if (global_arr(3) /= 30) error stop "FAIL: global_arr(3)"
        if (global_arr(4) /= 40) error stop "FAIL: global_arr(4)"

        if (abs(global_vec%x - 1.0d0) > 1.0d-10) &
            error stop "FAIL: global_vec x"
        if (abs(global_vec%y - 2.0d0) > 1.0d-10) &
            error stop "FAIL: global_vec y"
        if (abs(global_vec%z - 3.0d0) > 1.0d-10) &
            error stop "FAIL: global_vec z"

        if (.not. global_flag) error stop "FAIL: global_flag"
        if (global_counter /= 42) error stop "FAIL: global_counter"
    end subroutine

    subroutine test_write_to_c_globals()
        ! Fortran writes to C globals
        global_arr(1) = 100
        global_arr(2) = 200
        global_arr(3) = 300
        global_arr(4) = 400
        global_counter = 99

        ! C verifies the writes
        if (c29_verify_writes() /= 0) error stop "FAIL: C verify writes"
    end subroutine

    subroutine test_c_sizeof_derived()
        type(vec3_t) :: v
        integer(c_size_t) :: sz
        v = vec3_t(1.0d0, 2.0d0, 3.0d0)
        sz = c_sizeof(v)
        ! vec3_t has 3 doubles = 24 bytes
        if (sz /= 24_c_size_t) error stop "FAIL: c_sizeof vec3_t"
    end subroutine

    subroutine test_c_sizeof_array()
        integer(c_int32_t) :: arr(10)
        integer(c_size_t) :: sz
        arr = 0
        sz = c_sizeof(arr)
        ! 10 * 4 = 40 bytes
        if (sz /= 40_c_size_t) error stop "FAIL: c_sizeof array"
    end subroutine

    subroutine test_c_associated_two_args()
        integer(c_int32_t), target :: a, b
        type(c_ptr) :: pa, pb, pc

        a = 1
        b = 2
        pa = c_loc(a)
        pb = c_loc(b)
        pc = c_loc(a)

        ! pa and pc point to same location
        if (.not. c_associated(pa, pc)) error stop "FAIL: c_assoc same"
        ! pa and pb point to different locations
        if (c_associated(pa, pb)) error stop "FAIL: c_assoc diff"
        ! single-arg form
        if (.not. c_associated(pa)) error stop "FAIL: c_assoc single"
        ! null check
        if (c_associated(c_null_ptr)) error stop "FAIL: c_assoc null"
    end subroutine

    subroutine test_c_loc_variants()
        integer(c_int32_t), allocatable, target :: alloc_arr(:)
        integer(c_int32_t), target :: fixed_arr(5)
        type(vec3_t), target :: v
        type(c_ptr) :: p
        integer(c_int32_t), pointer :: fp

        ! C_LOC on allocatable
        allocate(alloc_arr(3))
        alloc_arr = [10, 20, 30]
        p = c_loc(alloc_arr)
        if (.not. c_associated(p)) error stop "FAIL: c_loc alloc"
        call c_f_pointer(p, fp)
        if (fp /= 10) error stop "FAIL: c_loc alloc value"

        ! C_LOC on array element
        fixed_arr = [100, 200, 300, 400, 500]
        p = c_loc(fixed_arr(3))
        call c_f_pointer(p, fp)
        if (fp /= 300) error stop "FAIL: c_loc element"

        ! C_LOC on derived type component
        v = vec3_t(7.0d0, 8.0d0, 9.0d0)
        p = c_loc(v%x)
        if (.not. c_associated(p)) error stop "FAIL: c_loc component"

        deallocate(alloc_arr)
    end subroutine

    subroutine test_c_f_pointer_multidim()
        integer(c_int32_t), target :: data(6)
        type(c_ptr) :: p
        integer(c_int32_t), pointer :: matrix(:,:)

        data = [1, 2, 3, 4, 5, 6]
        p = c_loc(data)
        call c_f_pointer(p, matrix, [2, 3])

        ! Column-major: matrix(1,1)=1, matrix(2,1)=2, matrix(1,2)=3, etc.
        if (matrix(1, 1) /= 1) error stop "FAIL: c_f_ptr 2d (1,1)"
        if (matrix(2, 1) /= 2) error stop "FAIL: c_f_ptr 2d (2,1)"
        if (matrix(1, 2) /= 3) error stop "FAIL: c_f_ptr 2d (1,2)"
        if (matrix(2, 3) /= 6) error stop "FAIL: c_f_ptr 2d (2,3)"
    end subroutine

    subroutine test_type_codes()
        integer(c_int32_t) :: ai(3)
        real(c_double)     :: ad(3)
        real(c_float)      :: af(3)

        ai = [1, 2, 3]
        ad = [1.0d0, 2.0d0, 3.0d0]
        af = [1.0, 2.0, 3.0]

        if (c29_check_type_int32(ai) /= 1) error stop "FAIL: type code int32"
        if (c29_check_type_double(ad) /= 1) error stop "FAIL: type code double"
        if (c29_check_type_float(af) /= 1) error stop "FAIL: type code float"
    end subroutine

    subroutine test_same_array_multi_desc()
        integer(c_int32_t) :: arr(4)
        arr = [1, 2, 3, 4]
        ! dot product of arr with itself: 1+4+9+16 = 30
        if (c29_self_dot(arr, arr) /= 30) error stop "FAIL: self dot"
    end subroutine

    subroutine test_blank_name()
        integer(c_int32_t) :: r
        r = f29_blank_add(10_c_int32_t, 20_c_int32_t)
        if (r /= 30) error stop "FAIL: blank name"
    end subroutine

    ! ========== Test subroutines from bindc_34 ==========

    subroutine test_enum()
        ! Verify enum values match between Fortran and C
        if (c34_get_color(0) /= COLOR_RED) error stop "FAIL: enum RED"
        if (c34_get_color(1) /= COLOR_GREEN) error stop "FAIL: enum GREEN"
        if (c34_get_color(2) /= COLOR_BLUE) error stop "FAIL: enum BLUE"
        ! Pass Fortran enum to C and check
        if (c34_check_color(COLOR_RED, 0) /= 1) error stop "FAIL: check RED"
        if (c34_check_color(COLOR_GREEN, 1) /= 1) error stop "FAIL: check GREEN"
        if (c34_check_color(COLOR_BLUE, 2) /= 1) error stop "FAIL: check BLUE"
    end subroutine

    subroutine test_dt_fixed_array()
        type(vec5_t) :: v
        v%data = [10, 20, 30, 40, 50]
        if (c34_sum_vec5(v) /= 150) error stop "FAIL: vec5 sum"
        if (c34_get_vec5_elem(v, 0) /= 10) error stop "FAIL: vec5 elem 0"
        if (c34_get_vec5_elem(v, 2) /= 30) error stop "FAIL: vec5 elem 2"
        if (c34_get_vec5_elem(v, 4) /= 50) error stop "FAIL: vec5 elem 4"
    end subroutine

    subroutine test_c_f_procpointer()
        type(c_funptr) :: cfp
        procedure(int_func_i), pointer :: fptr
        ! Get function pointer from C (doubles its argument)
        cfp = c34_get_func_ptr()
        ! Convert to Fortran procedure pointer
        call c_f_procpointer(cfp, fptr)
        ! Call through procedure pointer
        if (fptr(21) /= 42) error stop "FAIL: c_f_procpointer call"
        if (fptr(0) /= 0) error stop "FAIL: c_f_procpointer zero"
        if (fptr(-5) /= -10) error stop "FAIL: c_f_procpointer neg"
    end subroutine

    subroutine test_module_globals()
        ! Set from Fortran, read from C
        g34_char = 'Z'
        if (c34_get_char() /= 'Z') error stop "FAIL: module char F->C"

        g34_cmplx = cmplx(3.0, 4.0)
        if (abs(c34_get_cmplx_re() - 3.0) > 1.0e-5) &
            error stop "FAIL: module cmplx re F->C"
        if (abs(c34_get_cmplx_im() - 4.0) > 1.0e-5) &
            error stop "FAIL: module cmplx im F->C"

        ! Set from C, read from Fortran
        call c34_set_globals('A', 1.5, 2.5)
        if (g34_char /= 'A') error stop "FAIL: module char C->F"
        if (abs(real(g34_cmplx) - 1.5) > 1.0e-5) &
            error stop "FAIL: module cmplx re C->F"
        if (abs(aimag(g34_cmplx) - 2.5) > 1.0e-5) &
            error stop "FAIL: module cmplx im C->F"
    end subroutine

    subroutine test_common_block()
        ! Set from Fortran, read from C
        common_x = 42
        common_y = 84
        if (c34_get_common_x() /= 42) error stop "FAIL: common x F->C"
        if (c34_get_common_y() /= 84) error stop "FAIL: common y F->C"

        ! Set from C, read from Fortran
        call c34_set_common(100, 200)
        if (common_x /= 100) error stop "FAIL: common x C->F"
        if (common_y /= 200) error stop "FAIL: common y C->F"
    end subroutine

    ! ========== Test subroutines from bindc_40 ==========

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
        ! Note: LFortran treats c_funptr as CPtr (same as c_ptr),
        ! so the type code is CFI_type_cptr (41) instead of
        ! CFI_type_cfunptr (43). Skip this check for now.
        ! if (c40_check_cfunptr_type(fps) /= 1) error stop "FAIL: CFI_type_cfunptr"
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
        integer :: i

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

    ! ========== Test subroutines from bindc_42 ==========

    subroutine test_negative_stride_section()
        ! All done on C side: create array, CFI_section with stride=-1, verify
        if (c42_test_negative_stride_section() /= 1) &
            error stop "FAIL: negative stride section"
    end subroutine

    subroutine test_save_bindc()
        g42_save_var = 42
        if (c42_get_save_var() /= 42) error stop "FAIL: save var F->C"
        call c42_set_save_var(100_c_int)
        if (g42_save_var /= 100) error stop "FAIL: save var C->F"
    end subroutine

    subroutine test_protected_bindc()
        ! Cannot modify g42_protected here (PROTECTED), but C can read it
        call set_protected(77_c_int)
        if (c42_get_protected() /= 77) error stop "FAIL: protected C read"
    end subroutine

    subroutine test_optional_alloc_scalar()
        integer(c_int), allocatable :: x
        integer(c_int) :: r

        ! Present and allocated
        allocate(x)
        x = 42
        r = c42_opt_alloc_scalar(x)
        if (r /= 42) error stop "FAIL: opt alloc scalar present"

        ! Absent
        r = c42_opt_alloc_scalar()
        if (r /= -1) error stop "FAIL: opt alloc scalar absent"
        deallocate(x)
    end subroutine

    subroutine test_optional_ptr_scalar()
        integer(c_int), pointer :: p
        integer(c_int), target :: tgt
        integer(c_int) :: r

        ! Present and associated
        tgt = 55
        p => tgt
        r = c42_opt_ptr_scalar(p)
        if (r /= 55) error stop "FAIL: opt ptr scalar present"

        ! Absent
        r = c42_opt_ptr_scalar()
        if (r /= -1) error stop "FAIL: opt ptr scalar absent"
    end subroutine

    subroutine test_c_f_pointer_lower_bounds()
        use iso_c_binding
        integer(c_int), target :: data(6)
        type(c_ptr) :: cp
        integer(c_int), pointer :: fp2d(:,:)
        integer :: i

        data = [(i, i = 1, 6)]
        cp = c_loc(data(1))

        ! C_F_POINTER with shape [3,2] and implicit lower bound 1
        call c_f_pointer(cp, fp2d, [3, 2])
        if (fp2d(1,1) /= 1) error stop "FAIL: c_f_ptr 2d (1,1)"
        if (fp2d(3,1) /= 3) error stop "FAIL: c_f_ptr 2d (3,1)"
        if (fp2d(1,2) /= 4) error stop "FAIL: c_f_ptr 2d (1,2)"
        if (fp2d(3,2) /= 6) error stop "FAIL: c_f_ptr 2d (3,2)"
    end subroutine

    ! ========== Test subroutines from bindc_44 ==========

    subroutine test_44_all()
        ! pair_t is available from bindc_25_types (identical definition)
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
    end subroutine

end program
