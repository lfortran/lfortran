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

    type :: Base
        integer :: x
    end type Base

    type, extends(Base) :: Derived
        real :: r
    end type Derived

    type :: type_t
    end type type_t































contains

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
    character(:), allocatable :: x(2)
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
























    ! Use the space above to insert new declarations, and remove the line, so
    ! that the lines below do not shift, to keep the diff minimal.
    !
    ! Only put statements below. If you need to call a function, put it into a
    ! module above.

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

    print *, count(1)
    print *, count([2])

    a_real = [logical::]
    print *,size(a_real)

    print *, iparity(["a", "b"])
    print *, parity(["a", "b"])
    
    shape_ = [2, 3]
    matrix = reshape(source, shape_, pad=[0])

    deallocate(shape_)

    ! c is Complex
    print *, c%mymember
    ! c1 is Character
    print *, c1%mymember

    print *, present(x,x)
    print *, present()
    print *, ieor(x)
    print *, ieor()

    exit

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

    ieee_cls = ieee_class(0.0)
    b = (ieee_cls == ieee_quiet_nan)

    integer, intent(out) :: out_intent
    integer, intent(in) :: in_intent
    
    base_var = derived_var

















    
    contains
    subroutine sub(f)
        interface
            function f(x)
                integer :: x, f
            end function
        end interface
    end subroutine
end program
