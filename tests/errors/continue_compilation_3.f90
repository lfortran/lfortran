! If you need a function, put it into the module below and remove the same
! number of lines below the module to keep the rest of the lines in this file
! intact.
module continue_compilation_3_mod

    type :: tbp_ptr_dummy_t
    contains
        procedure :: m => tbp_ptr_dummy_f  ! Warning: passed-object dummy argument that is POINTER is not standard
    end type
    type :: tbp_ptr_dummy_pass_t
    contains
        procedure, pass(self) :: m => tbp_ptr_dummy_pass_f  ! Warning: passed-object dummy argument that is POINTER is not standard
    end type

contains

    subroutine check_incompatible_type(i)
      real :: i
    end subroutine check_incompatible_type

    subroutine intent_out_test(x)
        integer, intent(out) :: x
        x = 42
    end subroutine intent_out_test

    subroutine intent_inout_test(y)
        integer, intent(inout) :: y
        y = y + 1
    end subroutine intent_inout_test

    subroutine assumed_rank(x)
        integer, intent(in) :: x(..)
        print *, x
        print *, reshape(x, [size(x)])
        print *, x(1)
	    x = [1, 2]
        print *, (x /= [1, 2])
        print *, x + x
    end subroutine assumed_rank

    subroutine ss(x)
    character(:), allocatable :: x
    x = "AB"          ! allocates x with length 2
    end subroutine ss
    function tbp_ptr_dummy_f(this) result(r)
        class(tbp_ptr_dummy_t), pointer, intent(in) :: this; real :: r; r = 0.0
    end function
    function tbp_ptr_dummy_pass_f(self) result(r)
        class(tbp_ptr_dummy_pass_t), pointer, intent(in) :: self; real :: r; r = 0.0
    end function
end module continue_compilation_3_mod

! Only put declarations and statements here, no subroutines (those go above).
program continue_compilation_3
    use continue_compilation_3_fake_module
    use continue_compilation_3_mod

    ! Put declarations below without empty lines
    implicit none
    integer :: a(5), b(10), x1(3,2), c, a1(3), i, k = 3, x
    character(4) :: str(3)
    integer, parameter :: x3 = 2
    character(len=8) :: s, s1 = "lfortran"
    rea :: test_real(12)
    real :: test_re()
    int ::

    type :: t
        integer :: xx
    end type
    type(t), parameter :: cc_param_struct = t(4)
    integer, parameter :: cc_param_arr(3) = [1, 2, 3]
    type(t) :: y; type(tbp_ptr_dummy_t) :: tbp_noarg_w
    integer :: merge_i = 4, merge_j = 5
    integer(8) :: merge_k = 8
    integer :: arr_nc(minval(abs(a)))
    call intent_out_test(1)  ! Error: literal constant with intent(out)
    call intent_out_test(x + 1)  ! Error: expression with intent(out)
    call intent_inout_test(2)  ! Error: literal constant with intent(inout)
    call intent_inout_test(x * 2)  ! Error: expression with intent(inout)
    call check_incompatible_type(i)  ! Error: incompatible type passed

    print *, iachar(s1)
    
    type :: runner
        procedure(), nopass, pointer :: caller => null()
    contains
    end type



    ! array_01_cc
    a = []
    ! array_02_cc
    print *, [[[], [[]]], [[]], []]
    print *, [[[], [[]]], []]
    ! array_03_cc
    b(:,:) = 1
    b(:,:) = 2
    ! array_04_cc
    y%xx(:) = 1
    ! array_06_cc
    str(1, 2)(:) = '1234'
    str(1,2,3)(:) = '1234'
    ! array_constructor_with_asterisk_in_type_spec_cc
    print *, [character(*) :: "a", "b", "ball", "cat"]
    print *, [character(*) :: "a2", "b2", "ball2", "cat2"]
    !array_shape_01_cc
    x1 = reshape([1,2,3,4],[2,2])
    x1 = reshape([1,2,3,4],[1,2])
    !arithmetic_if1_cc
    i = -3
    c = 0
    if ("yy") 1, 2, 3
    1 c = c + 1
    2 c = c + 2
    3 c = c + 4
    print *, c
    if (c /= 7) error stop
    !array_size_01_cc
    i = size(a1, 1, 4, kind=4)
    i = size()
    !assign_01_cc
    x3 = 1
    print *, x3
    ! data_implied_do1
    data(a1(i), i=1, k) / 1, 2, 3 /
    ! data_implied_do2
    data(a1(i), i=1, 3, k) / 1, 2, 3 /
    ! data_implied_do3
    data(a1(i), i=k, 3) / 1, 2, 3 /

    !program_variable
    i = foo
    !rewind_invalid_kwarg1
    rewind(end="world")
    print *, ishft(3, 40)  !ishft_shift_too_large
    print *, log(0.0)  !log_zero
    !sign_01
    print *, sign(1, 1_8)
    !specific_type_intrinsic
    print*, dabs(1)
    !sqrt_neg
    print *, sqrt(-1.0)
    !string_binop
    print *, "a" + "b"
    !string_negative_start_index
    s = "lfortran"
    print*, "s:", s(-1:4)
    !string_slice
    print*, s1(-2:6)
    !string_slice2
    print*, "Length of s:", len(s)
    print*, s1(1: 9)
    !substring_noninteger_endidx
    print*, s1(1:5.2)
    !substring_noninteger_startidx
    print*, s1(1.1:5)
    !substring_noninteger_stride
    print*, s(1:5:2.2)
    !type_mismatch1
    x = "x"
    !type_mismatch2
    x = 5 + "x"
    !subroutine1, subroutine2, subroutine3, subroutine5
    call bpe()
    i = bpe()
    print *, xx
    test_re = 1245.13
    c(1) = 1

    integer :: i_incorrect_pragma
        !LF$unroll 4  ! Error: Missing space after `!LF$`
    do i_incorrect_pragma = 1, 10
        print *, i_incorrect_pragma
    end do
    !merge_bits
    print *, merge_bits(1, 2, 3_8)
    print *, merge_bits(merge_i,merge_j,merge_k)

    ! adding the new test for performing logical .and. between int-logical type.
    !and_mismatch_int_logical
    print *, 5 .and. .true.

    !and_mismatch_real_logical
    print *, 3.14 .and. .false.

    !and_okay_int_int
    print *, 5 .and. 6

    !adding a few more cases
    print *, 5 .or. 6
    print *, 5 .eqv. 6
    print *, .true. .neqv. 6
    print *, 3.14 .and. "abcd"
    print *, "abcd" .neqv. "cdef"
    print *, 1 .neqv. 2
    print *, [1,2,3] .and. .true.
    print *, [1.0, 2.0] .or. [3.0, 4.0]
    print *, "str1" .or. "str2"
    print *, "x" .and. .false.
    print *, .NOT. "lf"
    print *, "8356" .or. 8356
    print *, "8356" .eqv. 8356.00
    print *, ['c', 'o', 'd', 'e'] .or. ['m', 'a', 's']
    print *, ["welcome", "to", "lf"] .and. "contributors"  !even size diff of array element must be caught
    print *,  [( '*',i , i = 1, 10 )] 
    call ss("hello")

    !DATA on a PARAMETER (named constant) is a semantic error
    data cc_param_struct%xx / 5 /  ! {Error} PARAMETER 'cc_param_struct' shall not appear in a DATA statement
    data (cc_param_arr(i), i=1,3) / 4, 5, 6 /  ! {Error} PARAMETER 'cc_param_arr' shall not appear in a DATA statement
    i = tbp_noarg_w%m  ! Error: type-bound procedure referenced without an argument list
    contains 
    subroutine bpe()
        print *, size(bpe)
        bpe = d
    end subroutine
        
end program

module fref_m_13849
    implicit none
    type :: s_13849
        integer, allocatable :: a(:)
    end type
contains
    function mk_13849(i) result(r)
        integer, intent(in) :: i
        type(s_13849) :: r
        allocate(r%a(2))
        r%a = i
    end function
    subroutine test_fref_13849()
        print *, size(mk_13849(1)%a)
        print *, mk_13849(1)%a
    end subroutine
end module

! An intrinsic numeric operator applied to an operand of a derived type, or of
! `type(c_ptr)`, for which no operator is defined is an error.
subroutine cc3_derived_type_intrinsic_binop_error(c)
    use iso_c_binding, only: c_ptr
    implicit none
    type(c_ptr), intent(in) :: c
    type :: cc3_binop_t
        integer :: k = 2
    end type
    type(cc3_binop_t) :: x, y(3)
    print *, x + 1  ! {Error} Operator `+` undefined for the types in the expression `cc3_binop_t + integer(4)`
    print *, 2 - x  ! {Error} Operator `-` undefined for the types in the expression `integer(4) - cc3_binop_t`
    print *, x * x  ! {Error} Operator `*` undefined for the types in the expression `cc3_binop_t * cc3_binop_t`
    print *, 2.0 ** y  ! {Error} Operator `**` undefined for the types in the expression `real(4) ** cc3_binop_t[:]`
    print *, c / 2  ! {Error} Operator `/` undefined for the types in the expression `type(c_ptr) / integer(4)`
end subroutine cc3_derived_type_intrinsic_binop_error

! An intrinsic numeric operator with two logical operands is an error (#14735).
subroutine cc3_logical_intrinsic_binop_error()
    implicit none
    logical :: a, b(2)
    logical(8) :: c
    a = .true.
    b = .false.
    c = .true.
    print *, a + a  ! {Error} Operator `+` undefined for the types in the expression `logical(4) + logical(4)`
    print *, a - b  ! {Error} Operator `-` undefined for the types in the expression `logical(4) - logical(4)[:]`
    print *, b * a  ! {Error} Operator `*` undefined for the types in the expression `logical(4)[:] * logical(4)`
    print *, a / c  ! {Error} Operator `/` undefined for the types in the expression `logical(4) / logical(8)`
    print *, c ** a  ! {Error} Operator `**` undefined for the types in the expression `logical(8) ** logical(4)`
end subroutine cc3_logical_intrinsic_binop_error

! A defined operator applies only if the kinds of its dummy arguments agree
! with the operands; otherwise the intrinsic operator is used and rejected.
module cc3_operator_kind_mismatch_m
    implicit none
    type :: cc3_kind_t
        integer :: i = 0
    end type
    interface operator(+)
        module procedure cc3_kind_add
    end interface
    interface operator(==)
        module procedure cc3_kind_eq
    end interface
contains
    function cc3_kind_add(x, y) result(r)
        type(cc3_kind_t), intent(in) :: x
        integer(8), intent(in) :: y
        type(cc3_kind_t) :: r
        r%i = x%i + int(y)
    end function
    logical function cc3_kind_eq(x, y)
        type(cc3_kind_t), intent(in) :: x
        real(8), intent(in) :: y
        cc3_kind_eq = x%i == y
    end function
    subroutine cc3_kind_mismatch(x)
        type(cc3_kind_t), intent(in) :: x
        print *, x + 1  ! {Error} Operator `+` undefined for the types in the expression `cc3_kind_t + integer(4)`
        print *, x == 1.0  ! {Error} Operands of comparison operator '==' are not comparable: cc3_kind_t == real
    end subroutine
end module

subroutine upoly_type_name_14739(d)
    implicit none
    class(*), intent(in) :: d
    if (d) print *, "x"  ! {Error} Expected logical expression in if statement, but recieved class(*) instead
    print *, d == 1  ! {Error} Operands of comparison operator '==' are not comparable: class(*) == integer
end subroutine

subroutine assumed_type_name_14739(x)
    implicit none
    type(*), intent(in) :: x
    if (x) print *, "x"  ! {Error} Expected logical expression in if statement, but recieved type(*) instead
end subroutine

module upoly_to_specific_14738
    implicit none
    type :: t_14738
        integer :: x
    end type
contains
    subroutine g_int(i)
        integer, intent(in) :: i
        print *, i
    end subroutine
    subroutine g_t(a)
        type(t_14738), intent(in) :: a
        print *, a%x
    end subroutine
    integer function f_int(i)
        integer, intent(in) :: i
        f_int = i
    end function
end module

subroutine upoly_to_specific_type_14738(d)
    use upoly_to_specific_14738, only: t_14738, g_int, g_t, f_int
    implicit none
    class(*), intent(in) :: d
    integer :: i
    character(len=10) :: c
    type(t_14738) :: a
    class(*), allocatable :: u
    i = d  ! {Error} Type mismatch in assignment, the types must be compatible
    c = d  ! {Error} Type mismatch in assignment, the types must be compatible
    a = d  ! {Error} Type mismatch in assignment, the types must be compatible
    u = d
    call g_int(d)  ! {Error} Type mismatch in argument `i`: expected `integer(4)` but got `class(*)`
    call g_t(d)  ! {Error} Type mismatch in argument `a`: expected `t_14738` but got `class(*)`
    print *, f_int(d)  ! {Error} Type mismatch in argument `i`: expected `integer(4)` but got `class(*)`.
end subroutine
