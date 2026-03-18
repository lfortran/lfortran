! Test: Module variables, ISO_C_BINDING intrinsics, edge cases
!
! Covers gap items H.1-H.6, I.1, J.1-J.7, K.1-K.3, L.1:
!   - Module array variable with bind(C)
!   - Module derived type variable with bind(C)
!   - Module character variable with bind(C)
!   - Module complex variable with bind(C)
!   - Module logical variable with bind(C)
!   - Fortran writing to C global variables
!   - BIND(C, NAME="") blank name
!   - C_SIZEOF with derived types and arrays
!   - C_ASSOCIATED with two arguments
!   - C_LOC on allocatable, array element, derived type component
!   - C_F_POINTER with SHAPE for multi-dim
!   - CFI descriptor type code validation
!   - Passing same array to multiple descriptor args
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

        ! ---- descriptor type code query ----
        integer(c_int) function c29_get_type_code_int32(a) &
                bind(C, name="c29_get_type_code_int32")
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function

        integer(c_int) function c29_get_type_code_double(a) &
                bind(C, name="c29_get_type_code_double")
            import :: c_int, c_double
            real(c_double), intent(in) :: a(:)
        end function

        integer(c_int) function c29_get_type_code_float(a) &
                bind(C, name="c29_get_type_code_float")
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

program bindc_29
    use bindc_29_mod
    use bindc_29_globals
    use bindc_29_types
    use bindc_29_blank_name
    use iso_c_binding
    implicit none

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

    print *, "All bindc_29 tests passed."

contains

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
        integer :: tc

        ai = [1, 2, 3]
        ad = [1.0d0, 2.0d0, 3.0d0]
        af = [1.0, 2.0, 3.0]

        tc = c29_get_type_code_int32(ai)
        if (tc /= 9) error stop "FAIL: type code int32"

        tc = c29_get_type_code_double(ad)
        if (tc /= 28) error stop "FAIL: type code double"

        tc = c29_get_type_code_float(af)
        if (tc /= 27) error stop "FAIL: type code float"
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

end program
