! If you need a function, put it into the module below and remove the same
! number of lines below the module to keep the rest of the lines in this file
! intact.
module continue_compilation_1_mod
    use, intrinsic :: ieee_arithmetic, only: ieee_class, ieee_quiet_nan, ieee_class_type
    type :: MyClass
        integer :: value
    contains
        procedure :: display
    end type MyClass

    type :: logger_type
    contains
        private
        procedure, public, pass(self) :: add_log_file
    end type logger_type

    type(MyClass), PROTECTED :: protected_module_my_class_obj

    ! Test for Missing Declaration:
    type :: ctx_missing_t
        procedure(f_missing), pointer, nopass :: fn => null()
    end type

    procedure(missing_global_interface), pointer :: p => null()

    interface assignment(=)
        module procedure assign_func_bad
        module procedure assign_wrong_nargs
        module procedure assign_bad_lhs
        module procedure assign_bad_rhs
    end interface
    
    interface operator(.op.)
        function op_clash_f(x) result(y)
            integer, intent(in) :: x
            integer :: y
        end function
    end interface
    type :: Base
        integer :: x
    end type Base

    interface frexp
    function frexp(x,n) result(r)
        real r
        real, intent(in), value :: x
        integer, intent(out) :: n
    end function frexp
    end interface frexp

    interface frexp_duplicate
    subroutine frexp(x,n)
        real r
        real, intent(in), value :: x
        integer, intent(out) :: n
    end subroutine frexp
    end interface
    
    type, extends(Base) :: Derived
        real :: r
    end type Derived

    type :: type_t
    end type type_t


    type, extends(MyClass) :: derived_binding; contains; procedure :: display => display_override; end type  ! {Error} Type bound procedure 'display' of 'derived_binding' overriding the binding of the same name in 'myclass' must take 2 arguments, not 3
contains

    integer function statement_function_name_conflict()
        statement_function_name_conflict(argument) = 0
    contains
        integer function argument()
        end function
    end function
    subroutine my_undefined_type_test()
        implicit none
        type(another_undefined_type) :: s3_in_subroutine
    end subroutine my_undefined_type_test

    subroutine my_func(x, y)
        integer, intent(in) :: x, y
        print *, "hi"
    end subroutine

    subroutine display(self, extra_arg)
        class(MyClass), intent(in) :: self
        integer, intent(in) :: extra_arg
        print *, "Value in object:", self%value
    end subroutine display

    subroutine add_log_file(self, filename, unit)
        class(logger_type), intent(inout) :: self
        character(*), optional :: filename
        integer :: unit
        filename = "lfortran"
        unit = 10
    end subroutine add_log_file

    subroutine s(c) bind(c)
        use iso_c_binding
        character(len=2, kind=c_char), intent(in) :: c
    end subroutine s

    subroutine ubound_assumed_size(a, b, c)
        real :: a(*)       
        real :: b(*)   
        real :: c(10, *)
        
        print *, ubound(a, 1)
        print *, ubound(b)
        print *, ubound(c, 2)
    end subroutine

    subroutine assumed_size_star_pos_1(a)
        real, intent(in) :: a(*, 10)
    end subroutine

    subroutine assumed_size_star_pos_2(a)
        real :: a(*, 10)
    end subroutine

    subroutine proc_param(p)
        procedure(ubound_assumed_size) :: p
    end subroutine proc_param

    subroutine modify_and_deallocate(s)
        character(5), allocatable :: s
        deallocate(s)
    end subroutine modify_and_deallocate

    subroutine intrinsic_polymorphic(generic)
        class(*), intent(in) :: generic
        print *, trim(generic)
        print *, adjustl(generic)
        print *, adjustr(generic)
        print *, len_trim(generic)
    end subroutine intrinsic_polymorphic

    integer function assign_func_bad(lhs, rhs)
        integer, intent(out) :: lhs
        integer, intent(in)  :: rhs
        assign_func_bad = rhs
    end function assign_func_bad

    subroutine assign_wrong_nargs(lhs)
        integer, intent(out) :: lhs
    end subroutine assign_wrong_nargs

    subroutine assign_bad_lhs(lhs, rhs)
        integer, intent(in)  :: lhs
        integer, intent(in)  :: rhs
    end subroutine assign_bad_lhs

    subroutine assign_bad_rhs(lhs, rhs)
        integer, intent(out) :: lhs
        integer, intent(out) :: rhs
    end subroutine assign_bad_rhs
    subroutine slash_init_warning_paths()
        enum, bind(c)
            enumerator :: red/1/
        end enum
        type(MyClass), save :: slash_x/MyClass(1)/
        integer, save :: slash_y/2/
    end subroutine slash_init_warning_paths

    function dummy_func() result(r)
        integer :: r
        r = 42
    end function dummy_func

    subroutine dummy_sub()
       print *, "dummy subroutine"
    end subroutine dummy_sub

    subroutine proc_ptr_error_tests()
        implicit none
        procedure(), pointer :: pf1
        pf1 => dummy_sub

        procedure(sub_test), pointer :: pf2
        pf2 => dummy_func
    end subroutine proc_ptr_error_tests
    subroutine display_override(self, a, b); class(derived_binding), intent(in) :: self; integer, intent(in) :: a, b; end subroutine
    function op_clash_f(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x
    end function op_clash_f













end module


! Only put declarations and statements here, no subroutines (those go above).
program continue_compilation_1
    use continue_compilation_1_mod
    implicit integer(a-f), real(e-z)

    ! Put declarations below without empty lines
    integer :: a(3), b(3), b1(3, 3), a3(3, 3, 3), b4(3, 3, 3, 3), a5, c5, i, arr1(3), arr2(2, 3), arr3(2, 1, 3)
    character :: a1(3, 3)
    logical :: a2(3, 3), mask1(3), mask2(2, 3), mask3(2, 1, 3), mask4(3, 2), mask5(2, 3, 1), y
    integer(kind=8) :: b5
    real(8) :: y1
    real :: z1
    integer, parameter :: i1 = 2
    character(len=5) :: string = "hello"
    character(len=1) :: set(2) = ["l", "h"]
    integer :: q1
    real :: r1
    character :: c1
    complex :: c = (1.0, 2.0)
    real a_real(0)
    integer, allocatable ::  shape_(:), arr4(:), arr5(:)
    integer, dimension(2, 3) :: matrix
    integer, dimension(4) :: source = [1, 2, 3, 4]
    allocate(shape_(2))
    real :: v
    character(10) :: str
    character(3), parameter :: ar1 = repeat(["abc", "#^1", "123"], [1, 2, 3])
    integer, parameter :: zero = 0
    integer :: x = 1
    real :: adwf = .true.
    integer, volatile :: volatile_var
    dimension array(3)
    double precision array
    integer , dimension(3) :: array
    type(logger_type) :: logger
    integer :: unit
    character(len=100) :: filename
    type(MyClass), parameter :: myclass_array(2) = [1, MyClass(10)]
    type(MyClass), parameter :: myclass_array2(2) = [MyClass(1), MyClass(q1)]
    character(width=10) :: str_c_1
    character(len=10, len=20) :: str_c_2
    character(len=10, 1) :: str_c_3
    character(1, len=20) :: str_c_4
    character(:), allocatable :: z_01(2)
    integer, dimension(:,:), allocatable :: arr_size
    logical :: mask_size(size(arr_size))
    integer, protected :: protected_attr_var
    integer, parameter, protected :: protected_parameter_var
    type(MyClass) :: v1, v2, v3
    type(MyClass) :: arr(3)
    integer :: n = 2
    type :: matrix
      integer :: elements(n)
    end type
    type(bspline_3d) :: s3_in_program
    integer :: j2, i2, k2(2), x2(2), y2(3)    
    integer::tt = b'01' * 3
    integer :: fmt_i1, fmt_i2, fmt_i3 ! for issue #8925
    integer, allocatable :: allocate_int = 1
    character(:), allocatable :: allocate_char = "H"
    intrinsic :: not_real
    call sub(not_real)
    integer :: param_arr(3) = [5, 10, 15]
    integer, parameter :: param_minloc = minloc(param_arr, 1, [.false., .false., .false.])
    integer :: cc_a3(2) = [2, 3]
    integer :: cc_temp3(2)
    integer :: cc_i0 = 1
    integer :: cc_a4(2)
    integer :: cc_temp4(5)
    integer :: cc_i1 = 1
    character(10) :: strx
    type(MyClass), parameter :: uninitialized_param_local
    type(MyClass) :: err_obj1 = non_existent_symbol
    type(MyClass) :: err_obj2 = my_func
    integer :: non_parameter_var = 5
    type(MyClass) :: err_obj3 = non_parameter_var
    type(MyClass) :: err_obj4 = myclass_array
    type(MyClass) :: err_obj5 = uninitialized_param_local

    ! Unary defined operator with missing procedure
    interface operator(.bad.)
        module procedure bad_op
    end interface

    integer :: bad_x
    type(ieee_class_type) :: ieee_cls
    type(Base) :: base_var
    type(Derived) :: derived_var
    class(type_t) :: inst_tt
    real(8), parameter :: erfc_param = erfc(40.12_8)
    integer :: arr_idl(4)
    contiguous :: contig_not_declared
    contiguous :: MyClass
    class(Derived), allocatable :: derived_cls
    integer, parameter :: z(1) = 2
    integer, parameter :: qval(2) = reshape([7, 8], -[z])
    integer :: u
    type matrix(n)
        integer, len :: n
        real :: data(n)
    end type
    type(MyClass) :: eoshift_derived_array(1), eoshift_derived_result(1)












    ! Use the space above to insert new declarations, and remove the line, so
    ! that the lines below do not shift, to keep the diff minimal.
    !
    ! Only put statements below. If you need to call a function, put it into a
    ! module above.
    print 1+2
    a = 1
    print *, a(10)
    a5 = 8
    b5 = 12_8
    c5 = 2

    !loop_test
    do i=1,3
       i = i + 1
       print*,i
    end do
    !maskl_incorrect_bit_size
    print*, maskl(63)
    !maskr_incorrect_bit_size
    print*, maskr(63)
    !maskl_negative
    print*, maskl(-24)
    !maskr_negative
    print*, maskr(-24)
    !matrix_matmul_01
    print *, matmul(a1, b1)
    !matrix_matmul_02
    print *, matmul(b1, a1)
    !matrix_matmul_03
    print *, matmul(a2, b1)
    !matrix_matmul_04
    print *, matmul(a3, b1)
    !matrix_matmul_05
    print *, matmul(b1, b4)
    !matrix_matmul_06
    print *, matmul(a, b)
    !matrix_transpose_01
    print *, transpose(a)
    !merge_bits_comp
    print *, merge_bits(8, 12_8, 2)
    !merge_bits_run
    print *, merge_bits(a5, b5, c5)

    !Does not work correctly : Issue: #5469 -------------
    ! !max_01
    ! y1 = 5.2d0
    ! z1 = 9.0
    ! print *, max(y1, z1)
    ! !max_02
    ! print *, max(b5, a5)
    ! !min_01
    ! print *, min(y1, z1)
    ! !min_02
    ! print *, min(b5, a5)
    !------------------------------

    !modulo_01
    print *, modulo(1, 0)
    !more_kwargs_than_acceptable_to_subroutine
    call my_func(y=1, x=2, z=1)

    !nint_overflow
    print*, nint(1e12_8)
    print*, nint(1000000000000.0000000000000000d0)
    ! open_invalid_kwarg1
    OPEN(file="numbers", hello="world")
    !parameter_01
    i1 = 3
    print*,i1
    call FLUSH(1, 2)

    print*, verify(string, set, kind= [4, 4] )
    print *, and([1, 2, 3], [1, 2, 3])

    print *, dshiftl(1, 2, 34)
    print *, dshiftl(1, 2, -2)

    print *, dshiftr(1, 2, 34)
    print *, dshiftr(1, 2, -2)

    print *, selected_int_kind([1,2,3])
    print *, selected_real_kind([1,2,3])
    print *, selected_char_kind(['c', 'a', 'b'])

    arr1 = reshape([1, 2, 3], [3])
    arr2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    arr3 = reshape([1, 2, 3, 4, 5, 6], [2, 1, 3])
    mask1 = reshape([.true., .false., .true.], [3])
    mask2 = reshape([.true., .false., .true., .true., .false., .true.], [2, 3])
    mask3 = reshape([.true., .false., .true., .true., .false., .true.], [2, 1, 3])

    print *, sum(arr1, dim = 2)
    print *, sum(arr1, dim = -1)
    print *, sum(arr1, mask = mask1, dim = 2)
    print *, sum(arr1, mask = mask1, dim = -1)

    print *, product(arr2, dim = 3)
    print *, product(arr2, dim = -1)
    print *, product(arr2, mask = mask2, dim = 3)
    print *, product(arr2, mask = mask2, dim = -1)

    print *, iparity(arr3, dim = 4)
    print *, iparity(arr3, dim = -1)
    print *, iparity(arr3, mask = mask3, dim = 4)
    print *, iparity(arr3, mask = mask3, dim = -1)

    if (q1) q1 = 1
    if (r1) r1 = 1.0
    if (c1) c1 = 'a'

    mask4 = reshape([.true., .false., .true., .true., .false., .true.], [3, 2])
    mask5 = reshape([.true., .false., .true., .true., .false., .true.], [2, 3, 1])

    print *, sum(arr1, mask2)
    print *, sum(arr2, mask3, 2)
    print *, iparity(arr2, mask4)
    print *, iparity(arr3, mask5, 3)

    ! argument_not_a_variable
    print *, present(a + 1)

    ! argument_not_optional
    print *, present(a)

    print *, pack([1, 2, 3], [.true., .true., .true., .true.])

    print *, reshape("hello", [2, 3])
    print *, reshape(.true., [2, 3])
    print *, reshape([1, 2, 3, 4], "hello")
    print *, reshape([1, 2, 3, 4], .false.)

    print *, reshape([1, 2, 3, 4], [2, 3])

    ! Division by zero
    print *, 1/0
    print *, x/zero
    print *, v**str
    print *, str**v

    print *, shiftl(2, 34)
    print *, shiftl(2, -3)
    print *, shiftr(2, 34)
    print *, shiftr(2, -3)
    print *, rshift(2, 34)
    print *, rshift(2, -3)

    print *, sum([c1])
    print *, product([c1])
    print *, minval([c])
    print *, maxval([c])

    print *, sum(q1)
    print *, product(r1)
    print *, minval(q1)
    print *, maxval(r1)
    
    print *, sum([1, 2, 3], mask = [1, 2, 3])
    z1 = y 

    print *, reshape([1, 2, 3, 4, 5, 6], [2, 3], 0)
    print *, reshape([1, 2, 3, 4, 5, 6], [2, 3], [0], 0)
    print *, reshape([1, 2, 3, 4, 5, 6], [2, 3], [1.2])
    print *, reshape([1, 2, 3, 4, 5, 6], [2, 3], [0_8])

    print *, reshape([1, 2, 3, 4, 5, 6], [2, 3], order = [1.0, 2.0])
    print *, reshape([1, 2, 3, 4, 5, 6], [2, 3], order = [2, 3])
    print *, a(b'01':2)
    print *, count(1)
    print *, count([2])
    print *, a(1:2:b'10')
    a_real = [logical::]
    print *,size(a_real)
    print *, dummy_sub()
    print *, iparity(["a", "b"])
    print *, parity(["a", "b"])
    print *, string(1:6)
    shape_ = [2, 3]
    matrix = reshape(source, shape_, pad=[0])

    deallocate(shape_)

    ! c is Complex
    print *, c%mymember
    ! c1 is Character
    print *, c1%mymember
    print *, string(1:Z'100000003')
    print *, present(x,x)
    print *, present()
    print *, ieor(x)
    print *, ieor()
    print *, min(c, c)
    exit
    cycle
    ! calling function with less arguments
    call my_func(10)
    call my_func()
    ! checking for self argument too 
    type(MyClass) :: obj
    obj%value = 42
    call obj%display()
    ! checking source in allocate
    allocate(arr4(5), source=[1, 2, 3])
    allocate(arr4(5), source=v)
    allocate(arr4(3), source=reshape([1, 2, 3, 4, 5, 6], [2, 3]))
    allocate(arr4, source=7)

    call logger % add_log_file(filename=filename)
    call logger % add_log_file()

    allocate(arr5, status=q1)
    allocate(arr5, mold = arr4)

    print *, ["aa", "aaa"]
    cc_a3 = cc_temp3(cc_i0:cc_i0)
    print *, pack(arr2, mask1)
    print *, size(cc_a3)
    ! assigning to a *PROTECTED* struct instance member, not allowed
    protected_module_my_class_obj%value = 42
    cc_a4 = cc_temp4(cc_i1+1:cc_i1+1)
    arr = [type(MyClass) :: v1, v2, v3]
    print *, size(cc_a4)
    arr = [NonExistingType :: v1, v2, v3]

    !Data Statements with different number of arguments on LHS and RHS
    data j2, x2, (y2(i2), i2=1,3), k2 / 1,2,3,4,5,6,7,3*8 /

    q1: do q1 = 1, 3
        print *, q1
    end do q1

    ! Test assigned format WRITE 
    ASSIGN 0012 TO fmt_i1
    0012 FORMAT (" **** ASSIGN FORMAT NUMBER TO INTEGER VARIABLE ****" )
    WRITE (6, fmt_i1)

    ! Test assigned format PRINT 
    assign 100 to fmt_i2
    100 format (A)
    print fmt_i2, "test"

    ! Test assigned format READ 
    assign 13 to fmt_i3
    13 format ()
    read (5, fmt_i3)

    !passing non procedure to procedure parameter
    call proc_param(42)

    x = 9010
    read (*, end=x) x
    read (*, end=9011.0) x
    x = 9012
    read (*, err=x) x
    read (*, err=9013.0) x
    write (*, end=9014) x
9014 continue
    write (*, err=9015) x
9015 continue

    read(*, *, end=999) x   
    read(*, *, err=500) x
    
    OPEN(unit=10, recl=10, recl=20)
    OPEN(unit=10, recl=10.5)

    i = 1
    print *, string(i,i)
    
    allocate(strx)
    strx = "hello12345"
    call modify_and_deallocate(strx)
    print *, allocated(strx)

    call intrinsic_polymorphic("  Hello World  ")

    OPEN(unit=10, encoding="UTF-8", encoding="UTF-8")
    OPEN(unit=10, encoding=10)

    character(len=10) :: str_var
    read(str_var, rec=1) x
    write(str_var, rec=1) x
    read(unit=10, rec=1, rec=2) y
    write(unit=10, rec=1, rec=2) y
    read(10, rec=1.5) y
    write(10, rec=2.5) y

    ! unary defined operator with no matching function
    bad_x = .bad. 10
    bad_x = 5 .op. 3
    ieee_cls = ieee_class(0.0)
    b = (ieee_cls == ieee_quiet_nan)

    integer, intent(out) :: out_intent
    integer, intent(in) :: in_intent
    
    base_var = derived_var

    type :: container(rk, ik)
        integer, kind :: rk
        integer, kind :: ik
        integer(kind=ik)  :: i_val(20)
        real(kind=rk)     :: r_val(20)
    end type container

    type(container(4)) :: obj1
    type(container) :: obj2
    call set_caller(1)
    arr_idl = (i, i = 1, 4)
    integer :: minloc_shape_mismatch = minloc([2, 1, 3], 1, [.true., .false.])
    integer :: maxloc_shape_mismatch = maxloc([2, 1, 3], 1, [.true., .false.])
    write (*, "(a)", advance="hello") "Dothraki culture"
    print *, sum(arr1, dim = mask1)
    print*, ieee_is_nan(1.0)
    open(unit=7, decimal=1, decimal="comma")
    open(unit=7, decimal="POINT", decimal="comma")
    integer :: char_len_var = 10
    character(len = char_len_var) :: char_nonconst
    interface undeclared_iface
        module procedure undeclared_proc  ! {Error} Symbol 'undeclared_proc' not declared
    end interface
    eoshift_derived_result = eoshift(eoshift_derived_array, 1)
    integer, parameter :: n2 = "abc"
    type(MyClass) :: ptr_src_no_target
    type(MyClass), pointer :: ptr_requires_target => ptr_src_no_target
    type(Base), target :: ptr_tgt_base
    type(MyClass), pointer :: ptr_type_mismatch => ptr_tgt_base
    a(1) = .true.
    derived_cls = base_var
    call print_len_non_char("  Hello World  ")
    print  *, 9.99e+99
    a5 = missing_required_arg_func()
    integer :: m = 7
    dimension :: m(3)
    open(newunit=u, file="test.dat", status="replace", asynchronous=1)
    open(newunit=u, file="test.dat", status="replace", asynchronous="yes", asynchronous="no")
    integer :: eoshift_bad_shift(2, 2)
    eoshift_bad_shift = 1
    b1 = eoshift(b1, eoshift_bad_shift)
    contains
    subroutine test_uminus_struct()
        use continue_compilation_1_mod, only: MyClass
        implicit none
        type(MyClass) :: tt
        print *, -tt
    end subroutine




    subroutine sub(f)
        interface
            function f(x)
                integer :: x, f
            end function
        end interface
    end subroutine
    subroutine sub_do_undeclared()
        implicit none
        integer :: n(3)
        do k = 1, 3
            n(k) = 42
        end do
    end subroutine
    subroutine sub_real_logical_init()
        implicit none
        real :: adwf = .true.
    end subroutine
    subroutine sub_abs_array_index()
        implicit none
        integer(4) :: arr1(3) = [2471095, 820012001, 39024800]
        if (abs(arr1)(1) /= 2471095) error stop
    end subroutine

    subroutine print_len_non_char(generic)
        implicit none
        class(*), intent(in) :: generic
        integer :: a
        a = 5
        print *, len(generic)
        print *, len(a)
    end subroutine print_len_non_char

    subroutine sub_write_unit_bad_type()
        implicit none
        real :: r
        write(unit=r, fmt=*) "hello"  ! {Error} `unit` must be of type Integer or Character
    end subroutine sub_write_unit_bad_type

    subroutine sub_array_constant_character_to_integer()
        implicit none
        integer :: x(3)
        x = [character(len=3) :: "aa", "bb", "aa"]
    end subroutine sub_array_constant_character_to_integer

    subroutine Z_01_sub()
        integer,allocatable  :: x(3)
        integer,pointer  :: y(3)
    end subroutine

    integer function missing_required_arg_func(stat)
        integer, intent(out) :: stat
        missing_required_arg_func = 0
        stat = 0
    end function
    subroutine sub_common_block_nonconstant_lower_bound(n)
        implicit none
        integer, intent(in) :: n
        integer :: arr(n:10)
        common /common_nonconstant_lower_bound/ arr
    end subroutine sub_common_block_nonconstant_lower_bound

    subroutine sub_common_block_nonconstant_upper_bound(n)
        implicit none
        integer, intent(in) :: n
        integer :: arr(1:n)
        common /common_nonconstant_upper_bound/ arr
    end subroutine sub_common_block_nonconstant_upper_bound
    
    subroutine select_case_array_bound()
        implicit none
        integer :: n
        n = 1
        select case (n)
        case (:[2])
        end select
    end subroutine
    subroutine bindc_optional_value(a) bind(c)
        implicit none
        integer, optional, value :: a
    end subroutine
    subroutine sub_alternate_return_intrinsic()
        call cpu_time(*1)
1       continue
    end subroutine 
    subroutine sync_all_stat_wrong_type()
        implicit none
        character(len=10) :: cstat
        sync all (stat=cstat)  ! {Error} `stat` argument of `sync all` must be of type integer
    end subroutine
    subroutine sync_all_errmsg_wrong_type()
        implicit none
        integer :: imsg
        sync all (errmsg=imsg)  ! {Error} `errmsg` argument of `sync all` must be of type character
    end subroutine
    subroutine sync_all_stat_array()
        implicit none
        integer :: astat(3)
        sync all (stat=astat)  ! {Error} `stat` argument of `sync all` must be scalar
    end subroutine
    subroutine sync_all_stat_undeclared()
        implicit none
        sync all (stat=nosuch)  ! {Error} Variable 'nosuch' is not declared
    end subroutine
    subroutine assumed_size_to_pointer_dummy(x)
        integer :: x(*)
        call ptr_sink(x)  ! {Error} actual argument for 'x' cannot be an assumed-size array
    end subroutine
    subroutine ptr_sink(x)
        integer, pointer :: x(..)
    end subroutine
    subroutine select_case_complex()
        implicit none
        complex :: nn
        select case (nn)
        case default
        end select
    end subroutine
    subroutine select_case_real()
        implicit none
        real :: x
        select case (x)
        case default
        end select
    end subroutine

    subroutine lexical_intrinsic_nondefault_character()
        implicit none
        character(kind=4) :: glyph
        print *, lge("a", glyph)  
        print *, lgt("a", glyph)  
        print *, lle(glyph, "z")  
        print *, llt(glyph, "z")  
    end subroutine

    subroutine c_loc_default_component_initializer()
        use iso_c_binding, only: c_loc, c_ptr
        integer, target :: target_value
        type :: holder
            type(c_ptr) :: ptr = c_loc(target_value)
        end type
    end subroutine

    subroutine spread_dim_out_of_range()
        implicit none
        integer :: a(3)
        a = [1,2,3]
        print *, spread(a, 5, 2)
    end subroutine

    subroutine merge_kind_mismatch()
        implicit none
        integer(kind=8) :: a
        integer(kind=4) :: b
        a = 1
        b = 2
        print *, merge(a, b, .true.)
    end subroutine

    subroutine co_max_complex_arg()
        implicit none
        complex :: z
        call co_max(z)
    end subroutine

    subroutine cosum_invalid_argument_type()
        implicit none
        logical :: mask
        call co_sum(mask)
    end subroutine

    subroutine duplicate_statement_label()
1000    continue
1000    continue
    end subroutine

    subroutine select_type_nonpolymorphic()
        implicit none
        integer :: a
        select type (a)
        type is (integer)
            print *, a
        end select
    end subroutine
    subroutine character_kind_mismatch()
        implicit none
        character(kind=1) :: c1
        character(kind=4) :: c4
        print *, min(c1, c4)
    end subroutine

    subroutine sub_undefined_goto_label()
        implicit none
        goto 20  ! {Error} Label 20 is not defined
    end subroutine

    subroutine length_specifier_non_character()
        implicit none
        integer :: i*2
    end subroutine

    subroutine assumed_size_to_assumed_shape_forward(items)
        implicit none
        integer :: items(*)
        call consume_assumed_shape(items)  ! {Error} actual argument for 'items' cannot be an assumed-size array
    end subroutine
    subroutine consume_assumed_shape(items)
        implicit none
        integer :: items(:)
    end subroutine
    subroutine assumed_size_to_assumed_shape_function_forward(items)
        implicit none
        integer :: items(*)
        print *, consume_assumed_shape_function(items)  ! {Error} actual argument for 'items' cannot be an assumed-size array
    end subroutine
    integer function consume_assumed_shape_function(items)
        implicit none
        integer :: items(:)
        consume_assumed_shape_function = size(items)
    end function
    subroutine associate_boz_target()
        associate (y => z'1') 
        end associate
    end subroutine

    subroutine data_type_mismatch()
        implicit none
        integer :: x
        data x / "abc" /
    end subroutine
    subroutine norm2_error()
        real :: x
        x = norm2(1.0)
        x = norm2([1, 2])
        x = norm2([1.0, 2.0], dim=2)
    end subroutine

    subroutine type_used_before_declared_local()
        implicit none
        type(t_pair_local) :: x
        type :: t_pair_local
            integer :: i
            real :: x
        end type
    end subroutine type_used_before_declared_local

    subroutine real_unsupported_kind_01()
        print *, real(1., 666)
    end subroutine

    ! The AST keeps the source order, so with `--continue-compilation` the
    ! AST -> ASR visitors see the `use` and `implicit` statements below in
    ! their (invalid) position rather than hoisted to the front. The parser
    ! reports the ordering errors, the visitors must cope with the raw order.
    subroutine decl_order_after_decl()
        integer :: decl_order_first
        use iso_fortran_env, only: int32
        implicit none
        integer(int32) :: decl_order_second
        decl_order_second = decl_order_first
    end subroutine
    subroutine equivalence_nonconstant_subscript()
        implicit none
        integer :: a(3), b(3), i
        equivalence (a(i), b(1))  ! {Error} equivalence array bounds and subscripts must be constant
    end subroutine equivalence_nonconstant_subscript

    subroutine equivalence_common_scalar_array_element()
        implicit none
        integer :: cs, arr(3)
        common /equivalence_common_scalar/ cs
        equivalence (cs, arr(2))  ! {Error} equivalence between a common block variable and this array element is not implemented
    end subroutine equivalence_common_scalar_array_element

    subroutine equivalence_common_array_overrun()
        implicit none
        real :: first, second, alias(4)
        common /equivalence_common_overrun/ first, second
        equivalence (first, alias(1))  ! {Error} equivalence between a common block variable and this array element is not implemented
    end subroutine equivalence_common_array_overrun
    subroutine set_caller(this)
        class(MyClass) :: this
    end subroutine

    subroutine findloc_character_kind_mismatch()
        implicit none
        character(kind=4, len=1) :: names(1)
        character(kind=1, len=1) :: key
        print *, findloc(names, key)
    end subroutine

    subroutine implied_do_loop_variable_not_integer()
        implicit none
        real :: r_idx
        real :: values(3)
        values = [(real(r_idx), r_idx = 1, 3)]  ! {Error} The implied do loop variable 'r_idx' must be a scalar integer, not real(4)
        print *, values(1)
    end subroutine

    subroutine character_kind_mixing_concat()
        implicit none
        character(kind=4, len=3) :: wide
        character(len=3) :: narrow
        wide = 4_"abc"
        narrow = "xyz"
        print *, wide // narrow  ! {Error} operands of // must be character with the same kind, found character(4) and character(1)
    end subroutine

    subroutine character_kind_mixing_compare()
        implicit none
        character(kind=4, len=3) :: wide
        character(len=3) :: narrow
        wide = 4_"abc"
        narrow = "xyz"
        if (wide == narrow) print *, "eq"  ! {Error} operands of comparison operator '==' must be character with the same kind, found character(4) and character(1)
    end subroutine

    subroutine character_literal_kind_not_supported()
        implicit none
        character(len=4) :: s
        s = 3_"abc"  ! {Error} kind 3 is not supported for character, only 1 and 4 are
    end subroutine

    subroutine intrinsic_type_member_not_found()
        implicit none
        type :: itm_t
            complex :: c
            character(len=5) :: s
            integer :: i
        end type itm_t
        type :: itm_outer_t
            type(itm_t) :: in
        end type itm_outer_t
        type(itm_t) :: d
        type(itm_outer_t) :: o
        print *, d%c%bogus  ! {Error} Complex variable 'c' only has %re, %im, and %kind members, not 'bogus'
        print *, d%s%bogus  ! {Error} Character variable 's' only has %len and %kind members, not 'bogus'
        print *, d%i%bogus  ! {Error} Variable 'i' doesn't have any member named, 'bogus'.
        print *, o%in%c%bogus  ! {Error} Complex variable 'c' only has %re, %im, and %kind members, not 'bogus'
    end subroutine

    subroutine character_maxval_dim_mask_not_supported()
        implicit none
        character(len=3) :: c(2,2), r(2), s
        c = 'abc'
        r = maxval(c, dim=1)  ! {Error} `dim` and `mask` arguments to `MaxVal` are not implemented yet for arrays of character type
        r = minval(c, dim=1)  ! {Error} `dim` and `mask` arguments to `MinVal` are not implemented yet for arrays of character type
        s = maxval(c, mask=.false.)  ! {Error} `dim` and `mask` arguments to `MaxVal` are not implemented yet for arrays of character type
        s = minval(c, mask=.false.)  ! {Error} `dim` and `mask` arguments to `MinVal` are not implemented yet for arrays of character type
    end subroutine

    ! Fortran 2023 conditional expressions, ( cond ? a : b ). C1004 requires
    ! the arms to agree in declared type, kind type parameters and rank, and
    ! R1002 requires the condition to be a scalar logical expression.
    subroutine conditional_expr_arm_type_mismatch()
        implicit none
        integer :: x
        x = ( .true. ? 1 : 1.0 )  ! {Error} the arms of a conditional expression must have the same type and kind
    end subroutine

    subroutine conditional_expr_arm_kind_mismatch()
        implicit none
        integer(4) :: a
        integer(8) :: b, x
        character(kind=1, len=2) :: c1
        character(kind=4, len=2) :: c4
        character(len=2) :: c
        logical(4) :: l4
        logical(8) :: l8, l
        a = 1; b = 2
        x = ( .true. ? a : b )  ! {Error} the arms of a conditional expression must have the same type and kind
        c1 = "ab"; c4 = 4_"ab"
        c = ( .true. ? c1 : c4 )  ! {Error} the arms of a conditional expression must have the same type and kind
        l4 = .true.; l8 = .false.
        l = ( .true. ? l4 : l8 )  ! {Error} the arms of a conditional expression must have the same type and kind
    end subroutine

    subroutine conditional_expr_arm_rank_mismatch()
        implicit none
        integer :: a, b(2)
        a = 1; b = 2
        print *, ( .true. ? a : b )  ! {Error} the arms of a conditional expression must have the same rank
    end subroutine

    subroutine conditional_expr_arm_declared_type_mismatch()
        implicit none
        type :: cond_base
            integer :: n
        end type
        type, extends(cond_base) :: cond_ext
            integer :: m
        end type
        type(cond_base) :: b
        type(cond_ext) :: e
        b = cond_base(1); e = cond_ext(2, 3)
        print *, ( .true. ? e : b )  ! {Error} the arms of a conditional expression must have the same declared type
    end subroutine

    subroutine conditional_expr_condition_not_logical()
        implicit none
        integer :: x
        x = ( 1 ? 2 : 3 )  ! {Error} the condition of a conditional expression must be logical
    end subroutine

    subroutine conditional_expr_condition_not_scalar()
        implicit none
        logical :: m(2)
        integer :: a(2), b(2)
        m = .true.
        a = 1; b = 2
        print *, ( m ? a : b )  ! {Error} the condition of a conditional expression must be scalar
    end subroutine

    ! A conditional expression produces a value, so it is neither a target nor
    ! a pointer (R1033 requires a data-target).
    subroutine conditional_expr_pointer_assignment()
        implicit none
        integer, target :: a, b
        integer, pointer :: p
        a = 1; b = 2
        p => ( .true. ? a : b )  ! {Error} a conditional expression cannot be a pointer assignment target
    end subroutine

    ! A conditional expression is a primary (R1001), but 10.1.11 and 10.1.12
    ! enumerate the primaries a specification expression and a constant
    ! expression may contain, and a conditional expression is in neither list.
    ! The parenthesized alternative in those lists is the R1001 form
    ! `( expr )`, which is a different alternative of primary from R1002.
    ! Each declaration below is rejected, so its entity is deliberately left
    ! unused: referring to it would only add a cascading "not declared" error.
    subroutine conditional_expr_in_init_expr()
        implicit none
        integer, parameter :: cx = ( .true. ? 1 : 0 )  ! {Error} a conditional expression is not allowed in a constant expression
        integer :: cy = ( .true. ? 1 : 0 )  ! {Error} a conditional expression is not allowed in a constant expression
    end subroutine

    subroutine conditional_expr_in_kind()
        implicit none
        integer(kind = ( .true. ? 4 : 8 )) :: ck  ! {Error} a conditional expression is not allowed in a constant expression
        character(kind = ( .true. ? 1 : 4 ), len = 2) :: cc  ! {Error} a conditional expression is not allowed in a constant expression
    end subroutine

    subroutine conditional_expr_in_specification_expr(n)
        implicit none
        integer, intent(in) :: n
        character(len = ( n>0 ? n : 1 )) :: cs  ! {Error} a conditional expression is not allowed in a specification expression
        integer :: ca( ( n>0 ? n : 1 ) )  ! {Error} a conditional expression is not allowed in a specification expression
    end subroutine

    subroutine derived_type_constructor_argument_errors()
        implicit none
        type :: plain_t
            integer :: value
        end type
        type :: parameterized_t(k)
            integer, kind :: k
            integer :: value
        end type
        print *, parameterized_t(4, 8)(1)  ! {Error} too many arguments in parameterized derived type constructor
        print *, parameterized_t(4)(1, 2)  ! {Error} too many arguments in parameterized derived type constructor
        print *, plain_t(1, 2)  ! {Error} too many arguments in derived type constructor
        print *, parameterized_t(4, 1, 2)  ! {Error} too many arguments in derived type constructor
    end subroutine

    ! Intrinsic assignment of an array constructor to a scalar. With a
    ! defined assignment(=) in scope this is legal (see integration test
    ! defined_assignment_02.f90); without one it stays an error.
    subroutine array_constructor_to_scalar()
        implicit none
        integer :: i
        i = [1, 2, 3]  ! {Error} ArrayInitalizer expressions can only be assigned array references
    end subroutine

    ! Keep the unsupported character kind declarations last: a rejected
    ! declaration makes the symbol table visitor skip the program units that
    ! follow it, which would hide the errors expected above.
    subroutine character_kind_not_supported()
        implicit none
        character(kind=2, len=4) :: a  ! {Error} kind 2 is not supported for character, only 1 and 4 are
        character(kind=3, len=4) :: b  ! {Error} kind 3 is not supported for character, only 1 and 4 are
        character(kind=8, len=4) :: c  ! {Error} kind 8 is not supported for character, only 1 and 4 are
    end subroutine
end program

! A syntax error inside a module makes the parser skip the erroneous
! declaration and keep the rest of the module. The symbol table visitor then
! skips the program units that depend on the discarded declaration, so the body
! visitor must not assume their symbols exist.
module module_error_recovery_1
    type :: t_recovery
    contains
      foo    end type t_recovery
contains
    pure function foo(self, x) result(res)
      class(t_recovery), intent(in) :: self
      real, intent(in) :: x(:)
      real :: res(size(x))
    end function foo
end module

! An arm of a conditional expression is an ordinary expression, so a call in
! it is still a call made by the enclosing procedure (15.7).
module conditional_expr_purity_1
    implicit none
contains
    integer function conditional_expr_impure()
        print *, "side effect"
        conditional_expr_impure = 1
    end function

    pure integer function conditional_expr_pure(c)
        logical, intent(in) :: c
        conditional_expr_pure = ( c ? 1 : conditional_expr_impure() )  ! {Error} Call to impure procedure 'conditional_expr_impure' is not allowed inside a PURE procedure
    end function
end module

! A statement label is a positive integer, so a leading zero is not a label at
! all and must be rejected by the tokenizer.
subroutine zero_statement_label_1()
    implicit none
    0 print *, "unreachable"  ! {Error} Zero is not a valid statement label
end subroutine

! A type bound procedure of a derived type declared outside a module must
! still name a module procedure or an external procedure with an explicit
! interface. Two program units declaring a derived type of the same name with
! the same binding must each be diagnosed on their own.
subroutine binding_outside_module_1
    type :: t_binding_outside_module
    contains
        procedure, pass(this) :: binding_outside_module_proc  ! {Error} 'binding_outside_module_proc' must be a module procedure or an external procedure with an explicit interface
    end type t_binding_outside_module
end subroutine binding_outside_module_1

subroutine binding_outside_module_2
    type :: t_binding_outside_module
    contains
        procedure, pass(this) :: binding_outside_module_proc  ! {Error} 'binding_outside_module_proc' must be a module procedure or an external procedure with an explicit interface
    end type t_binding_outside_module
end subroutine binding_outside_module_2

! `decimal=` on a data transfer statement takes a character value, and like any
! other specifier it may appear at most once in the control list.
subroutine decimal_specifier_1()
    implicit none
    write(*, *, decimal=1) 1.0
    write(*, *, decimal="POINT", decimal="COMMA") 1.0
end subroutine

! A common block fixes the storage of its variables, so equivalencing two of
! them either contradicts that layout or associates two different blocks.
module equivalence_two_commons_1
    implicit none
contains
    subroutine equivalence_two_common_arrays()
        real :: lhs(4), rhs(4)
        common /equivalence_two_commons/ lhs, rhs
        equivalence (lhs(1), rhs(1))  ! {Error} equivalence between two common block variables is not allowed
    end subroutine
end module

! A `type(...)` entity can only be initialized with a value of its own type.
module init_type_mismatch_1
    implicit none
    type :: init_mismatch_a_t
        integer :: h = 0
    end type
    type :: init_mismatch_b_t
        integer :: h = 0
    end type
    type(init_mismatch_a_t), parameter :: init_mismatch_pa = init_mismatch_a_t(1)
    integer, parameter :: init_mismatch_ip = 3
    type(init_mismatch_b_t) :: init_mismatch_mv = init_mismatch_pa  ! {Error} type mismatch in initialization
contains
    subroutine init_type_mismatch_local()
        type(init_mismatch_b_t) :: x = init_mismatch_pa  ! {Error} type mismatch in initialization
        type(init_mismatch_a_t) :: y = init_mismatch_ip  ! {Error} type mismatch in initialization
        type(integer) :: i = init_mismatch_pa  ! {Error} type mismatch in initialization
    end subroutine
end module

! Initializing an entity from an imported parameter does not make a module
! export the parameter's type, whether it was imported under another name or
! only inside a procedure.
module imported_init_export_a
    implicit none
    type :: imported_init_t
        integer :: i = 0
    end type
    type(imported_init_t), parameter :: imported_init_z = imported_init_t(7)
end module

module imported_init_export_b
    use imported_init_export_a, only: imported_init_u => imported_init_t, imported_init_z
    implicit none
    type(imported_init_u) :: imported_init_mv = imported_init_z
contains
    integer function imported_init_local()
        use imported_init_export_a, only: imported_init_t, imported_init_z
        type(imported_init_t) :: x = imported_init_z
        imported_init_local = x%i
    end function
end module

subroutine imported_init_no_export()
    use imported_init_export_b
    implicit none
    type(imported_init_t) :: y  ! {Error} derived type `imported_init_t` is not defined
end subroutine

! A structure constructor argument is the value of its component, so an array
! argument must have the component's rank and extents.
subroutine structure_constructor_argument_shape_1()
    implicit none
    type :: t_constructor_shape
        integer :: a(3)
        integer :: s
        integer :: m(2, 2)
    end type
    type(t_constructor_shape), parameter :: p1 = &
        t_constructor_shape([1, 2], 1, reshape([1, 2, 3, 4], [2, 2]))  ! {Error} component 'a' has extent 3 in dimension 1, but the structure constructor argument has extent 2
    type(t_constructor_shape) :: v1 = t_constructor_shape([1, 2, 3], [1, 2], reshape([1, 2, 3, 4], [2, 2]))  ! {Error} component 's' has rank 0, but the structure constructor argument has rank 1
    type(t_constructor_shape) :: v2
    integer :: b(2), c(3, 2)
    b = 1
    c = 1
    v2 = t_constructor_shape(b, 1, reshape([1, 2, 3, 4], [2, 2]))  ! {Error} component 'a' has extent 3 in dimension 1, but the structure constructor argument has extent 2
    v2 = t_constructor_shape([1, 2, 3], 1, c)  ! {Error} component 'm' has extent 2 in dimension 1, but the structure constructor argument has extent 3
    v2 = t_constructor_shape(c, 1, reshape([1, 2, 3, 4], [2, 2]))  ! {Error} component 'a' has rank 1, but the structure constructor argument has rank 2
end subroutine

! `null()` is a disassociated pointer or an unallocated allocatable, so it
! cannot be the value of a component that is neither.
subroutine structure_constructor_null_component_1()
    implicit none
    type :: t_null_inner
        integer :: k
    end type
    type :: t_null_component
        integer :: x
        type(t_null_inner) :: in
        character(len=2) :: c
        integer, pointer :: p
    end type
    type(t_null_component), parameter :: p1 = t_null_component(null(), t_null_inner(1), "ab", null())  ! {Error} null() cannot be the value of component 'x' of type integer(4), which is neither a pointer nor allocatable
    type(t_null_component) :: v1 = t_null_component(1, null(), "ab", null())  ! {Error} null() cannot be the value of component 'in' of type type(t_null_inner), which is neither a pointer nor allocatable
    type(t_null_component) :: v2
    v2 = t_null_component(1, t_null_inner(1), c=null(), p=null())  ! {Error} null() cannot be the value of component 'c' of type character(len=2), which is neither a pointer nor allocatable
end subroutine

subroutine structure_constructor_null_component_2()
    implicit none
    type :: t_null_inner_2
        integer :: k
    end type
    type :: t_null_array_component
        real(8) :: x(2)
        type(t_null_inner_2) :: ins(3, 2)
    end type
    type(t_null_array_component) :: v1 = t_null_array_component(null(), t_null_inner_2(1))  ! {Error} null() cannot be the value of component 'x' of type real(8), dimension(2), which is neither a pointer nor allocatable
    type(t_null_array_component) :: v2
    v2 = t_null_array_component(1.0d0, ins=null())  ! {Error} null() cannot be the value of component 'ins' of type type(t_null_inner_2), dimension(3, 2), which is neither a pointer nor allocatable
end subroutine

! `c_null_ptr` and `c_null_funptr` are valid for a plain `type(c_ptr)` or
! `type(c_funptr)` component, but `null()` is not.
subroutine structure_constructor_null_component_3()
    use iso_c_binding, only: c_ptr, c_funptr, c_null_ptr, c_null_funptr
    implicit none
    type :: t_null_c_component
        integer :: h
        type(c_ptr) :: p = c_null_ptr
        type(c_funptr) :: f = c_null_funptr
    end type
    type(t_null_c_component), parameter :: p1 = t_null_c_component(1, null())  ! {Error} null() cannot be the value of component 'p' of type type(c_ptr), which is neither a pointer nor allocatable
    type(t_null_c_component) :: v1
    v1 = t_null_c_component(1, c_null_ptr, f=null())  ! {Error} null() cannot be the value of component 'f' of type type(c_funptr), which is neither a pointer nor allocatable
end subroutine

! The same for a parameterized derived type and for an extended type, whose
! parent components come first.
subroutine structure_constructor_null_component_4()
    use iso_c_binding, only: c_ptr, c_null_ptr
    implicit none
    type :: t_null_c_pdt(k)
        integer, kind :: k
        integer(k) :: h
        type(c_ptr) :: p = c_null_ptr
    end type
    type :: t_null_c_base
        integer :: h
        type(c_ptr) :: q = c_null_ptr
    end type
    type, extends(t_null_c_base) :: t_null_c_ext
        type(c_ptr) :: p
    end type
    type(t_null_c_pdt(4)) :: a
    type(t_null_c_ext) :: e
    a = t_null_c_pdt(4)(h=1, p=null())  ! {Error} null() cannot be the value of component 'p' of type type(c_ptr), which is neither a pointer nor allocatable
    e = t_null_c_ext(1, c_null_ptr, null())  ! {Error} null() cannot be the value of component 'p' of type type(c_ptr), which is neither a pointer nor allocatable
end subroutine

! `null()` for an integer component of a parameterized derived type.
subroutine structure_constructor_null_component_5()
    implicit none
    type :: t_null_int_pdt(k)
        integer, kind :: k
        integer(k) :: h
        integer :: j
    end type
    type(t_null_int_pdt(4)) :: a
    a = t_null_int_pdt(4)(null(), 2)  ! {Error} null() cannot be the value of component 'h' of type integer(4), which is neither a pointer nor allocatable
    a = t_null_int_pdt(4)(h=null(), j=2)  ! {Error} null() cannot be the value of component 'h' of type integer(4), which is neither a pointer nor allocatable
end subroutine

! A null constant whose type the component does not accept: `c_null_ptr` for
! an integer or real component, and `null(mold)` with a mold of another type.
subroutine structure_constructor_null_component_6()
    use iso_c_binding, only: c_null_ptr, c_null_funptr
    implicit none
    type :: t_null_mismatch
        integer, pointer :: ip
        integer, allocatable :: ia(:)
        real, pointer :: rp
        integer :: h
        real :: r
    end type
    type :: t_null_mismatch_pdt(k)
        integer, kind :: k
        integer(k) :: h
    end type
    type(t_null_mismatch) :: v
    type(t_null_mismatch_pdt(4)) :: a
    integer, pointer :: ip
    v = t_null_mismatch(c_null_ptr, null(), null(), 1, 1.0)  ! {Error} type mismatch in structure constructor: a null value of type type(c_ptr) cannot be the value of component 'ip' of type integer(4)
    v = t_null_mismatch(null(), c_null_ptr, null(), 1, 1.0)  ! {Error} type mismatch in structure constructor: a null value of type type(c_ptr) cannot be the value of component 'ia' of type integer(4), dimension(:)
    v = t_null_mismatch(null(), null(), null(ip), 1, 1.0)  ! {Error} type mismatch in structure constructor: a null value of type integer(4) cannot be the value of component 'rp' of type real(4)
    v = t_null_mismatch(null(), null(), null(), c_null_ptr, 1.0)  ! {Error} type mismatch in structure constructor: a null value of type type(c_ptr) cannot be the value of component 'h' of type integer(4)
    v = t_null_mismatch(null(), null(), null(), 1, r=c_null_funptr)  ! {Error} type mismatch in structure constructor: a null value of type type(c_ptr) cannot be the value of component 'r' of type real(4)
    a = t_null_mismatch_pdt(4)(c_null_ptr)  ! {Error} type mismatch in structure constructor: a null value of type type(c_ptr) cannot be the value of component 'h' of type integer(4)
end subroutine

subroutine derived_type_scalar_broadcast_parameter_array_oob()
    implicit none
    type :: t_scalar_broadcast_oob
        integer :: h
    end type
    type(t_scalar_broadcast_oob), parameter :: a(2) = t_scalar_broadcast_oob(7)
    type(t_scalar_broadcast_oob), parameter :: b(0:1) = t_scalar_broadcast_oob(8)
    type(t_scalar_broadcast_oob), parameter :: c(0:1, -2:-1) = t_scalar_broadcast_oob(9)
    integer, parameter :: k1 = a(3)%h  ! {Error} Array index 3 is out of bounds (1 to 2) in dimension 1
    integer, parameter :: k2 = a(0)%h  ! {Error} Array index 0 is out of bounds (1 to 2) in dimension 1
    integer, parameter :: k3 = a(-1)%h  ! {Error} Array index -1 is out of bounds (1 to 2) in dimension 1
    integer, parameter :: k4 = b(-1)%h  ! {Error} Array index -1 is out of bounds (0 to 1) in dimension 1
    integer, parameter :: k5 = c(0, 0)%h  ! {Error} Array index 0 is out of bounds (-2 to -1) in dimension 2
end subroutine

module scalar_struct_array_shape_errors_1
    implicit none
contains
    subroutine scalar_struct_array_assumed_size(a)
        type :: scalar_shape_t
            integer :: i
        end type
        type(scalar_shape_t) :: a(*) = scalar_shape_t(1)  ! {Error} array of derived type initialized with a scalar structure constructor must have constant explicit shape
    end subroutine

    subroutine scalar_struct_array_nonconstant_extent(n)
        integer, intent(in) :: n
        type :: scalar_shape_t
            integer :: i
        end type
        type(scalar_shape_t) :: a(n) = scalar_shape_t(1)  ! {Error} array of derived type initialized with a scalar structure constructor must have constant explicit shape
    end subroutine
end module

subroutine associate_constant_selector_assignment()
    implicit none
    type :: associate_const_a_t
        integer :: x
    end type
    type :: associate_const_b_t
        type(associate_const_a_t) :: a
        integer :: y
    end type
    type(associate_const_b_t), parameter :: pb = associate_const_b_t(associate_const_a_t(10), 30)
    type(associate_const_b_t), parameter :: pba(1) = [associate_const_b_t(associate_const_a_t(11), 31)]

    associate (q => pb)
        q = associate_const_b_t(associate_const_a_t(1), 2)  ! {Error} Cannot assign to a constant variable
    end associate

    associate (q => pb)
        q%a%x = 5  ! {Error} Cannot assign to a constant variable
    end associate

    associate (r => pb%a)
        r%x = 6  ! {Error} Cannot assign to a constant variable
    end associate

    associate (q => pba)
        q(1)%a%x = 7  ! {Error} Cannot assign to a constant variable
    end associate
end subroutine

subroutine associate_nested_constant_selector_assignment()
    implicit none
    type :: associate_nested_const_a_t
        integer :: x
    end type
    type :: associate_nested_const_b_t
        type(associate_nested_const_a_t) :: a
        integer :: y
    end type
    type(associate_nested_const_b_t), parameter :: pb = associate_nested_const_b_t(associate_nested_const_a_t(10), 30)
    type(associate_nested_const_b_t), parameter :: pba(1) = [associate_nested_const_b_t(associate_nested_const_a_t(11), 31)]

    associate (q => pb)
        associate (r => q)
            r%y = 88  ! {Error} Cannot assign to a constant variable
        end associate
    end associate

    associate (q => pb)
        associate (r => q%a)
            r%x = 99  ! {Error} Cannot assign to a constant variable
        end associate
    end associate

    associate (q => pb)
        associate (r => q%a%x)
            r = 77  ! {Error} Cannot assign to a constant variable
        end associate
    end associate

    associate (q => pba)
        associate (r => q(1))
            r%a%x = 66  ! {Error} Cannot assign to a constant variable
        end associate
    end associate

    associate (q => pb)
        associate (r => q)
            associate (s => r)
                s%y = 55  ! {Error} Cannot assign to a constant variable
            end associate
        end associate
    end associate
end subroutine

subroutine associate_parameter_array_selector_assignment()
    implicit none
    type :: associate_param_array_a_t
        integer :: x
    end type
    type :: associate_param_array_b_t
        type(associate_param_array_a_t) :: a
        integer :: y
    end type
    type(associate_param_array_b_t), parameter :: pba(2) = [ &
        associate_param_array_b_t(associate_param_array_a_t(11), 31), &
        associate_param_array_b_t(associate_param_array_a_t(12), 32)]
    integer :: i

    i = 1

    associate (r => pba(1))
        r = associate_param_array_b_t(associate_param_array_a_t(1), 2)  ! {Error} Cannot assign to a constant variable
    end associate

    associate (r => pba(i))
        r = associate_param_array_b_t(associate_param_array_a_t(1), 2)  ! {Error} Cannot assign to a constant variable
    end associate

    associate (r => pba(1:2))
        r(1) = associate_param_array_b_t(associate_param_array_a_t(1), 2)  ! {Error} Cannot assign to a constant variable
    end associate

    associate (r => pba(1)%a)
        r = associate_param_array_a_t(1)  ! {Error} Cannot assign to a constant variable
    end associate

    associate (r => pba(1)%a%x)
        r = 1  ! {Error} Cannot assign to a constant variable
    end associate
end subroutine

subroutine cptr_funptr_mismatch()
    use iso_c_binding, only: c_ptr, c_funptr, c_null_ptr, c_null_funptr
    implicit none
    type(c_ptr) :: cp
    type(c_funptr) :: fp
    type :: cptr_funptr_t
        type(c_ptr) :: p
        type(c_funptr) :: f
    end type
    type(cptr_funptr_t) :: v
    cp = c_null_funptr  ! {Error} Type mismatch in assignment, the types must be compatible
    fp = c_null_ptr  ! {Error} Type mismatch in assignment, the types must be compatible
    v = cptr_funptr_t(c_null_funptr, c_null_funptr)  ! {Error} type mismatch in structure constructor: a null value of type type(c_funptr) cannot be the value of component 'p' of type type(c_ptr)
    v = cptr_funptr_t(c_null_ptr, c_null_ptr)  ! {Error} type mismatch in structure constructor: a null value of type type(c_ptr) cannot be the value of component 'f' of type type(c_funptr)
end subroutine

subroutine cptr_funptr_intrinsic_result_mismatch()
    use iso_c_binding, only: c_int, c_ptr, c_funptr, c_loc, c_funloc
    implicit none
    interface
        subroutine cptr_funptr_bindc_target() bind(c)
            import
        end subroutine
        function returns_c_ptr_for_mismatch() result(r)
            import c_ptr
            type(c_ptr) :: r
        end function
    end interface
    integer(c_int), target :: x
    type(c_ptr) :: cp
    type(c_funptr) :: fp
    fp = c_loc(x)  ! {Error} Type mismatch in assignment, the types must be compatible
    cp = c_funloc(cptr_funptr_bindc_target)  ! {Error} Type mismatch in assignment, the types must be compatible
    fp = returns_c_ptr_for_mismatch()  ! {Error} Type mismatch in assignment, the types must be compatible
end subroutine

subroutine null_assignment_nonpointer_cptr()
    use iso_c_binding, only: c_ptr
    implicit none
    type(c_ptr) :: p
    p = null()  ! {Error} null() cannot be assigned to an entity of type type(c_ptr), which is not a pointer
end subroutine

subroutine pointer_component_constructor_target()
    use iso_c_binding, only: c_ptr, c_null_ptr
    implicit none
    type :: cptr_pointer_component_t
        type(c_ptr), pointer :: cp
    end type
    type :: integer_pointer_component_t
        integer, pointer :: ip
    end type
    integer :: v
    type(cptr_pointer_component_t) :: a
    type(integer_pointer_component_t) :: b
    a = cptr_pointer_component_t(c_null_ptr)  ! {Error} the value of pointer component 'cp' must be a pointer, a target or null()
    b = integer_pointer_component_t(1)  ! {Error} the value of pointer component 'ip' must be a pointer, a target or null()
    b = integer_pointer_component_t(v)  ! {Error} the value of pointer component 'ip' must be a pointer, a target or null()
end subroutine

subroutine cptr_component_constructor_type_mismatch()
    use iso_c_binding, only: c_ptr
    implicit none
    type :: payload_t
        integer :: k
    end type
    type :: cptr_component_t
        type(c_ptr) :: p
    end type
    type(payload_t), parameter :: payload = payload_t(1)
    type(cptr_component_t) :: a
    a = cptr_component_t(payload)  ! {Error} type mismatch in structure constructor: value of type type(payload_t) cannot be the value of component 'p' of type type(c_ptr)
    a = cptr_component_t(1)  ! {Error} type mismatch in structure constructor: value of type integer(4) cannot be the value of component 'p' of type type(c_ptr)
end subroutine

subroutine cfunptr_diagnostic_type_name()
    use iso_c_binding, only: c_funptr
    implicit none
    type(c_funptr) :: f
    call takes_integer(f)  ! {Error} Type mismatch in argument `x`: expected `integer(4)` but got `type(c_funptr)`
    if (f) print *, "bad"  ! {Error} Expected logical expression in if statement, but recieved type(c_funptr) instead
contains
    subroutine takes_integer(x)
        integer, intent(in) :: x
    end subroutine
end subroutine

subroutine null_initializer_nonpointer_integer()
    implicit none
    integer :: i = null()  ! {Error} null() cannot initialize 'i' of type integer(4), which is neither a pointer nor allocatable
end subroutine

subroutine null_initializer_nonpointer_cptr()
    use iso_c_binding, only: c_ptr
    implicit none
    type(c_ptr) :: p = null()  ! {Error} null() cannot initialize 'p' of type type(c_ptr), which is neither a pointer nor allocatable
end subroutine

subroutine null_initializer_nonpointer_component()
    implicit none
    type :: null_init_t
        integer :: k = null()  ! {Error} null() cannot initialize 'k' of type integer(4), which is neither a pointer nor allocatable
    end type
end subroutine

subroutine derived_type_constructor_too_many_null_args()
    implicit none
    type :: plain_t
        integer :: value
    end type
    type :: parameterized_t(k)
        integer, kind :: k
        integer :: value
    end type
    type(plain_t) :: pv = plain_t(1, null())  ! {Error} too many arguments in derived type constructor
    print *, plain_t(1, null())  ! {Error} too many arguments in derived type constructor
    print *, parameterized_t(4, 1, null())  ! {Error} too many arguments in derived type constructor
    print *, parameterized_t(4, null())(1)  ! {Error} too many arguments in parameterized derived type constructor
    print *, parameterized_t(4)(1, null())  ! {Error} too many arguments in parameterized derived type constructor
end subroutine

subroutine parent_component_keyword_conflicts()
    implicit none
    type :: pck_base_t
        integer :: x
    end type
    type, extends(pck_base_t) :: pck_e_t
        integer :: z
    end type
    type, extends(pck_e_t) :: pck_f_t
        integer :: w
    end type
    type(pck_e_t) :: e
    type(pck_e_t) :: ee(2)
    type(pck_base_t) :: arr(2)
    type(pck_f_t) :: f
    integer :: i
    e = pck_e_t(pck_base_t=pck_base_t(11), x=3, z=51)  ! {Error} component 'x' is already specified by the parent component 'pck_base_t'
    e = pck_e_t(x=3, pck_base_t=pck_base_t(11), z=51)  ! {Error} component 'x' is already specified, it cannot also be given by the parent component 'pck_base_t'
    e = pck_e_t(pck_base_t=42, z=51)  ! {Error} type mismatch in structure constructor: the parent component 'pck_base_t' requires a scalar value of type type(pck_base_t), not integer(4)
    e = pck_e_t(pck_base_t=arr, z=51)  ! {Error} type mismatch in structure constructor: the parent component 'pck_base_t' requires a scalar value of type type(pck_base_t), not type(pck_base_t), dimension(2)
    e = pck_e_t(pck_base_t=f, z=51)  ! {Error} type mismatch in structure constructor: the parent component 'pck_base_t' requires a scalar value of type type(pck_base_t), not type(pck_f_t)
    ee = [ (pck_e_t(pck_base_t=pck_make(i), z=i), i = 1, 2) ]  ! {Error} the value given for the parent component 'pck_base_t' must be a constant or a variable inside an implied do loop, it would otherwise be evaluated once for every component of 'pck_base_t'
contains
    function pck_make(i) result(res)
        integer, intent(in) :: i
        type(pck_base_t) :: res
        res = pck_base_t(i)
    end function
end subroutine

! `null()` is not permitted as the TARGET= argument to the `associated`
! intrinsic.
subroutine associated_null_target_in_continue_compilation_1()
    implicit none
    integer, pointer :: a(:)
    a => null()
    if (associated(a, null())) print *, "bad"  ! {Error} NULL() is not permitted as the TARGET= argument to 'associated'
end subroutine

! Fortran 2023 10.1.11: a specification expression is a restricted expression.
! An object designator is a permitted primary only when its base object is a
! dummy argument, is in a common block, or is made accessible by use or host
! association. A variable local to the same scoping unit is none of those, so
! it may not size another local or give one a length. A named constant, and an
! inquiry such as `size` or `len` about a local, stay permitted.
subroutine local_in_specification_expr_in_continue_compilation_1(n, s)
    implicit none
    integer, intent(in) :: n
    character(len=*), intent(in) :: s
    integer, parameter :: lse_p = 3
    type :: lse_t
        integer :: x
    end type
    type(lse_t), save :: lse_a(1) = lse_t(4)
    integer :: lse_m
    integer :: lse_c
    common /lse_blk/ lse_c
    integer :: ok_dummy(n)
    integer :: ok_common(lse_c)
    integer :: ok_param(lse_p)
    integer :: ok_inquiry(size(ok_dummy))
    character(len=n) :: ok_str
    character(len=len(ok_str)) :: ok_len
    character(len=len(s)) :: ok_assumed
    integer :: bad_member(lse_a(1)%x)  ! {Error} the variable 'lse_a' is local to this scoping unit, so it cannot appear in a specification expression
    integer :: bad_scalar(lse_m)  ! {Error} the variable 'lse_m' is local to this scoping unit, so it cannot appear in a specification expression
    character(len=lse_m) :: bad_len  ! {Error} the variable 'lse_m' is local to this scoping unit, so it cannot appear in a specification expression
    print *, size(ok_dummy), size(ok_param), size(ok_inquiry), size(ok_common)
    print *, len(ok_str), len(ok_len), len(ok_assumed)
end subroutine

! `w%u(2)` with `w` an array takes one element of the component out of every
! element of the base, so what it denotes is strided by the size of an element
! of `w`. Passing it would hand the callee the elements that follow the first
! one in memory instead, and an `intent(inout)` or `intent(out)` dummy would
! write them back. It is rejected until the argument is built by gathering the
! elements it names.
subroutine element_of_array_component_as_argument_in_continue_compilation_1()
    implicit none
    type :: eac_t
        integer :: u(3)
    end type
    type(eac_t) :: w(2)
    w(1)%u = [1, 2, 3]
    w(2)%u = [4, 5, 6]
    print *, w%u(2)
    print *, size(w%u(2))
    call eac_inout(w%u(2))  ! {Error} Passing an element of an array component of an array as an argument is not supported yet
    call eac_in(w%u(2))  ! {Error} Passing an element of an array component of an array as an argument is not supported yet
contains
    subroutine eac_inout(a)
        integer, intent(inout) :: a(:)
        a = -a
    end subroutine
    subroutine eac_in(a)
        integer, intent(in) :: a(:)
        print *, a
    end subroutine
end subroutine

module partial_template_instantiation
    implicit none
    template tmpl {t}
        deferred type :: t
    contains
        function identity(x) result(y)
            type(t), intent(in) :: x
            type(t) :: y
            y = x
        end function
        function outer(x) result(y)
            type(t), intent(in) :: x
            type(t) :: y
            y = identity(x)
        end function
    end template
    instantiate tmpl {real}, only: outer_real => outer, missing_symbol  ! {Error} Symbol missing_symbol was not found
    instantiate tmpl {integer}, only: outer_integer => outer
end module

module template_scope_restrictions_m
    implicit none
    template unary{t, op}
        deferred type :: t
        deferred interface
            function op(x) result(value)
                type(t), intent(in) :: x
                type(t) :: value
            end function
        end interface
    end template
contains
    subroutine check_forward_restrictions()
        instantiate unary{integer, scalar}
        ! Check every restriction against the completed actual, not its provisional interface.
        instantiate unary{real, scalar}  ! {Error} Restriction type mismatch with provided function argument
        instantiate unary{integer, real_result}  ! {Error} Restriction type mismatch with provided function argument
        instantiate unary{integer, binary}  ! {Error} Number of arguments mismatch, restriction expects a function with 1 parameters, but a function with 2 parameters is provided
        instantiate unary{integer, assign_value}  ! {Error} The restriction argument assign_value should have a return value
    contains
        integer function scalar(x) result(value)
            integer, intent(in) :: x
            value = x
        end function
        real function real_result(x) result(value)
            integer, intent(in) :: x
            value = real(x)
        end function
        integer function binary(x, y) result(value)
            integer, intent(in) :: x, y
            value = x + y
        end function
        subroutine assign_value(x)
            integer, intent(in) :: x
        end subroutine
    end subroutine
end module

module template_scope_recovery_m
    implicit none
    template unary{t, op}
        deferred type :: t
        deferred interface
            function op(x) result(value)
                type(t), intent(in) :: x
                type(t) :: value
            end function
        end interface
        type :: holder
            type(t) :: value
        end type
    contains
        function apply(x) result(value)
            type(t), intent(in) :: x
            type(t) :: value
            value = op(x)
        end function
    end template
contains
    subroutine check_rejected_bodies()
        instantiate unary{real, scalar}, only: rejected_apply => apply, rejected_holder => holder ! {Error} Restriction type mismatch with provided function argument
        instantiate unary{integer, scalar}, only: apply_scalar => apply
        instantiate unary{integer, real_result}, only: apply_real_result => apply ! {Error} Restriction type mismatch with provided function argument
        instantiate unary{integer, binary}, only: apply_binary => apply ! {Error} Number of arguments mismatch, restriction expects a function with 1 parameters, but a function with 2 parameters is provided
        instantiate unary{integer, assign_value}, only: apply_assign_value => apply ! {Error} The restriction argument assign_value should have a return value
        integer, parameter :: offset = 20
        type(rejected_holder) :: item
        procedure(rejected_apply), pointer :: rejected_callback
        procedure(apply_scalar), pointer :: callback

        item%value = 1.0
        callback => apply_scalar
        if (scalar(2) /= 22) error stop
        if (apply_scalar(3) /= 23) error stop
        if (callback(4) /= 24) error stop
        print *, after_template_recovery_missing ! {Error} Variable 'after_template_recovery_missing' is not declared
    contains
        integer function scalar(x) result(value)
            integer, intent(in) :: x
            value = x + offset
        end function
        real function real_result(x) result(value)
            integer, intent(in) :: x
            value = real(x)
        end function
        integer function binary(x, y) result(value)
            integer, intent(in) :: x, y
            value = x + y
        end function
        subroutine assign_value(x)
            integer, intent(in) :: x
        end subroutine
    end subroutine
end module

module template_scope_templated_function_m
    implicit none
    template unary{op}
        deferred interface
            integer function op(x)
                integer, intent(in) :: x
            end function
        end interface
    contains
        integer function apply(x) result(value)
            integer, intent(in) :: x
            value = op(x)
        end function
    end template
contains
    subroutine check_templated_function()
        instantiate unary{abs}, only: rejected_function => apply ! {Error} templated procedure 'abs' cannot be used as a procedure argument
        instantiate unary{increment}, only: valid_function => apply
        if (valid_function(2) /= 3) error stop
        print *, after_templated_function_missing ! {Error} Variable 'after_templated_function_missing' is not declared
    contains
        template function abs{t}(x) result(value)
            deferred type :: t
            type(t), intent(in) :: x
            type(t) :: value
            value = x
        end function
        integer function increment(x) result(value)
            integer, intent(in) :: x
            value = x + 1
        end function
    end subroutine
end module

module template_scope_templated_subroutine_m
    implicit none
    template action{op}
        deferred interface
            subroutine op(x)
                integer, intent(inout) :: x
            end subroutine
        end interface
    contains
        subroutine apply(x)
            integer, intent(inout) :: x
            call op(x)
        end subroutine
    end template
contains
    subroutine actual(x)
        integer, intent(inout) :: x
        x = x + 10
    end subroutine
    subroutine check_templated_subroutine()
        instantiate action{op=actual}, only: rejected_subroutine => apply ! {Error} templated procedure 'actual' cannot be used as a procedure argument
        instantiate action{assign_value}, only: valid_subroutine => apply
        integer :: value
        value = 2
        call valid_subroutine(value)
        if (value /= 3) error stop
        print *, after_templated_subroutine_missing ! {Error} Variable 'after_templated_subroutine_missing' is not declared
    contains
        template subroutine actual{t}(x)
            deferred type :: t
            type(t), intent(inout) :: x
            x = x
        end subroutine
        subroutine assign_value(x)
            integer, intent(inout) :: x
            x = x + 1
        end subroutine
    end subroutine
end module
