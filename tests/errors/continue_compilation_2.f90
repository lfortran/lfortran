module Geometry
    implicit none

    type :: Circle
        real :: radius
contains
    procedure :: calculateArea
    end type Circle

contains

    ! Type-bound subroutine to calculate the area of a circle
    subroutine calculateArea(self, area)
       class(Circle), intent(in) :: self
       real, intent(out) :: area
       area = 3.14 * self%radius**2
    end subroutine calculateArea
end module Geometry

module my_module
    integer :: x = 10
end module wrong_module_name

subroutine myname
end subroutine myname

block data myname
end block data myname

module continue_compilation_2_mod
    contains

    subroutine solsy ()
        double precision rowns(209)
        common /rowns/ rowns(209)
        print *, set_exponent([1, 2, 3], 2)
    end
    
    subroutine try_to_change(y)
        integer, intent(in) :: y
        y = 99  
    end subroutine

    subroutine my_subroutine1()
        print *, "Inside subroutine"
    end subroutine different_name

    function my_function() result(res)
        integer :: res
        res = 42
    end function not_my_function

    subroutine my_subroutine2()
        print *, "Inside subroutine"
    end subroutine different_name 

    SUBROUTINE faulty_subroutine(a, b, c)
        INTEGER, INTENT(IN) :: sub_a
    END SUBROUTINE faulty_subroutine


























    
end module continue_compilation_2_mod



! Only put declarations and statements here, no subroutines (those go above).
program continue_compilation_2
    use continue_compilation_2_mod
    use iso_fortran_env
    use iso_c_binding, only: c_ptr, c_f_pointer
    use Geometry
    implicit real(a-z)

    ! Put declarations below without empty lines
    integer, pointer, parameter :: v => null()
    integer, allocatable, parameter :: v=1
    integer init_x = 1

    ! Variable declarations 
    type(c_ptr) :: queries_1
    integer, pointer :: y_1
    type(c_ptr) :: queries_2
    integer(2), pointer :: y_2(:)
    integer :: shape(2, 2)
    integer, parameter :: x = 2
    type(Circle) :: myCircle
    real :: circleArea
    complex :: a
    integer :: val
    character(1) :: x_2
    integer :: i
    integer :: a_2(3)
    integer :: size_a
    integer :: a_3(3)
    integer :: size_a_2
    integer :: kindvar = 4
    integer :: atom[*]
    real(8), allocatable :: x_3(:)
    real :: y_3
    integer, parameter :: Nx = 600, Ny = 450
    integer :: i_1, j, image(Nx, Ny)
    integer :: i_2, j_1
    integer :: i_3
    complex :: a_4
    complex :: a_5
    real :: y_4
    integer :: idaa2_x(1:2,1:2,1:2)
    integer :: idaa2_y(1:2,1:2,1:1)
    logical, parameter :: idlalb1_x(3) = [.true., .false., .false.]
    logical, parameter :: idlalb2_x1(3) = [.true., .false., .false.]
    logical, parameter :: idlalb2_x2(2) = [.true., .true.]
    integer, allocatable :: iraa1_arr1(:, :)
    integer, allocatable :: iraa2_arr1(:, :, :)
    integer, allocatable :: iraa2_arr3(:)
    integer :: iatw1_b(5)
    integer :: iatw2_i1(5)
    integer :: iatw2_b(5)
    integer  :: itw1_b(5)
    integer  :: itw2_b(5)
    integer  :: itw3_b(5)
    INTEGER :: intent_x
    real(8) :: intr2_x, intr2_y, datan2
    integer(4) :: intr8_x = 1
    integer(8) :: intr8_y = 2
    integer(4) :: intr9_x = 1
    integer(8) :: intr9_y = 2
    integer(4) :: intr10_x = 1
    integer(8) :: intr10_y = 2
    integer, parameter :: ici_ios = 1
    character(len=100) :: ici_buffer
    integer :: insv_ios(2) = 1
    character(len=100) :: insv_buffer
    complex :: complex_z = (1, 2)
    integer :: tm1_x
    integer :: tm2_x
    !int_01_1.f90
    integer(8), parameter :: ar1(3) = int([1, 2, 3], [8, 8, 8])
    !int_01_2.f90
    integer(8), parameter :: ar2(3) = int([1, 2, 3], [8, 8, 8])
    !kind_invalid_float_of_int
    integer(4.2) :: ifoix
    !kind_invalid_int_of_complex
    complex(6) :: iiocx
    !kind_invalid_int_of_int
    integer(3) :: iifix
    !kind_invalid_int_of_logical
    logical(10) :: iiolx
    !kind_star_of_complex
    complex(*) :: ksoca
    !kind_star_of_int
    integer(*) :: ksoia
    !kind_star_of_logical
    logical(*) :: ksola
    !kind_string_of_int
    integer('a') :: ksoix
    !kind_var_of_int
    integer :: kvoia = 4
    real(kvoia) :: kvoix
    !kind1
    real(3) :: x
    !kind2
    real(*) kind2_a
    !type_conflict1
    integer, parameter, target :: foo=4
    integer :: x_bad_implicit
    !unsupported kind
    real*16 :: unsupported_kind
    ! argument not specified
    type(Circle) :: myCircle2 = Circle()
    ! invalid keyword argument specified
    type(Circle) :: myCircle3 = Circle(mykeyword=10)
    !tokenizer error
    integer  :: ? tokenizer_error
    integer, dimension(3,2) :: m = [ 1, 0, 0, 2, 4, 6 ]








    




























    ! c_f_pointer_01
    call c_f_pointer(queries_1, y_1, [2])
    ! c_f_pointer_02
    call c_f_pointer(queries_2, y_2, shape)
    ! assign_01
    x = 1
    ! class_procedure_extra_args
    myCircle%radius = 5.0
    call myCircle%calculateArea(circleArea, 12)
    ! close_invalid_kwarg1
    CLOSE(end=200)
    ! cmplx_01
    a = cmplx(y = 2)
    ! cmplx_02
    print*, cmplx((real(1, kind=4), 0.00000000), kind=8)
    ! cmplx_03
    print*, cmplx((1.00000000, real(0, kind=4)), kind=8)
    ! coarray_01
    val = this_image ()
    call co_sum (val, result_image=1)
    if (this_image() == 1) then
      write(*,*) "The sum is ", val                                
    end if
    ! coarray_02
    call event_query(1, 1, 1)
    ! compare_01
    x_2 = 'u'
    i = 10
    if (i > x_2) then
    else
    end if
    !array_size_02
    size_a = size(a_2, 1, dim=1)
    size_a = size(a_2, dim = 1, 1)
    !array_size_05
    size_a_2 = size(a_3, kind=kindvar, dim=1)
    size_a_2 = size(a_3, kind=kindvar)
    !atomic_01
    call atomic_add (atom[1], this_image())
    call atomic_add (atom[2], this_image())
    !array_constructor_with_asterisk_in_type_spec
    print *, [character(*) :: "a", "b", "ball", "cat"]
    !array_constructor_with_different_char_length
    print *, ["a", "b", "ball", "cat"]
    print *, ["a1", "b1", "ball1", "cat1"]
    !array_constructor_with_different_kind
    allocate(x_3(4))
    print *, [x_3, [1., 2.]]
    !array_constructor_with_different_types
    print *, [1, 2.]
    !array_constructor_with_integer_real_array_types
    print *, [1, [1., 2.]]
    !dfloat1
    print *, dfloat(y_3)
    !dim_float_01
    print *, sum([1, 2, 3], 1.1)
    !dim_float_02
    print *, sum([1, 2, 3], 1, 1.1)
    !dim_float_03
    print *, sum([1, 2, 3], .true., 1.1)
    !dint_args
    print*, dint(1.0_8, 8)
    if (abs(dint(1.0_8, 8) - 1.0_8) > 10e-5 ) error stop
    !dlgama
    print *, dlgama(2.7)
    !dnint_args
    print*, dnint(1.0_8, 8)
    if (abs(dnint(1.0_8, 8) - 1.0_8) > 10e-5 ) error stop
    !do_concurrent_01
    do concurrent (j = 1:Ny) local(i_1, j)
        do i_1 = 1, Nx
        end do
    end do
    !do_loop_01
    do i_2 = 1, 10
        do j_1 = 1, 2
        i_2 = j_1 + 1
        end do
        j_1 = i_2 + 1
        print *, i_2, j_1
    end do
    !do_zero_increment
    do i_2 = 1, 5, 0
        write(*,*) i_3
    end do
    !dprod
    print*, dprod(4.23_8, 4.3_8)
    !dreal_arg_error
    a_4 = (1.0, 2.0)
    print *, dreal(a_4)
    !fixed_number_of_args
    a_5 = complex(1)
    !float1
    print *, float(y_4)
    !flush_invalid_kwarg
    FLUSH(unit=10, start=100)
    !func_parameter_type
    print *, f(42.9)
    !ichar_01
    print*, ichar("okay")
    !idint_real4
    print *, idint(4.5)
    !ifix_01
    print *, ifix(4.23_8)
    !incompatible_dimension_assignment_arr1
    integer :: arr1(1)
    arr1 = [1, 2, 3]
    !incompatible_dimension_assignment_arr2
    idaa2_x = reshape([1, 2, 3, 4, 5, 6, 7, 8], [2, 2, 2])
    idaa2_y = reshape([1, 2, 3, 4], [2, 2, 1])
    idaa2_y = idaa2_x
    !incompatible_dimension_logical_arrays_logical_binop_01
    print *, idlalb1_x .neqv. [.true., .true.]
    !incompatible_dimension_logical_arrays_logical_binop_02
    print *, idlalb2_x1 .neqv. idlalb2_x2
    !incompatible_rank_allocatable_arr1
    iraa1_arr1 = [1, 2, 3]
    !incompatible_rank_allocatable_arr2
    iraa2_arr3 = iraa2_arr1
    !incorrect_array_type_where_01
    where([1, 2, 3, 4, 5]) iatw1_b = 1
    print *, iatw1_b
    if (all(iatw1_b /= [1, 0, 1, 0, 1])) error stop
    !incorrect_array_type_where_02
    iatw2_i1 = [1, 2, 3, 4, 5]
    where(iatw2_i1) iatw2_b = 1
    print *, iatw2_b
    if (all(iatw2_b /= [1, 0, 1, 0, 1])) error stop
    !incorrect_type_where_01
    where(.true.) itw1_b = 12121
    print *, itw1_b
    !incorrect_type_where_02
    where(1) itw2_b = 12121
    print *, itw2_b
    !incorrect_type_where_03
    where(max(1.33, 2.67)) itw3_b = 12121
    print *, itw3_b
    !intent1
    intent_x = 42
    CALL try_to_change(intent_x)
    !intrinsics1
    print *, radix((2.4, 1.0))
    !intrinsics2
    intr2_x = 2.33D0
    intr2_y = 3.41D0
    print *, datan2(x,y)
    if(abs(datan2(x,y) - 0.59941916594660438) > 1d-6) error stop
    !intrinsics3
    print *, ibclr(1, -2)
    !intrinsics4
    print *, dshiftl(1, 1_8, 1)
    !intrinsics5
    print *, ior(1, 1_8)
    !intrinsics6
    print *, ieor(1, 1_8)
    !intrinsics7
    print *, hypot(1.0, 2.7_8)
    !intrinsics8
    print *, ior(intr8_x, intr8_y)
    !intrinsics9
    print *, iand(intr9_x, intr9_y)
    !intrinsics10
    print *, ieor(intr10_x, intr10_y)
    !intrinsics11
    real(4) :: intr11_x = 1
    real(8) :: intr11_y = 2
    print *, hypot(intr11_x, intr11_y)
    !intrinsics12
    print *, max(12, 13.94)
    !intrinsics13
    print *, min(12, 13.94)
    !intrinsics14
    print *, scale([1, 2, 3], 2)
    !intrinsics15
    print *, set_exponent([1, 2, 3], 2)
    !iostat_constant_integer
    ici_buffer = 'Temporary date for testing purpose'
    read(ici_buffer, *, iostat=ici_ios)
    !iostat_non_scalar_value
    insv_buffer = 'Temporary date for testing purpose'
    read(insv_buffer, *, iostat=insv_ios(1:1))
    !ishftc_size
    print *, ishftc(10, 6, 4)
    !complex_01
    print *, cmplx(complex_z , 1)
    !kind_01
    print *, aint([1.0, 2.0, 3.0], [4, 4])
    !type_mismatch_1
    tm1_x = "x"
    !type_mismatch_2
    tm2_x = 5 + "x"

    print *,foo
    x_bad_implicit = 10
    print *, x_bad_implicit

    ! member not found
    print *, myCircle%mymember

    100 FORMAT(A10, @)

    print*, merge("okay", "ok", .true.)

    contains
    logical function f(x)
        integer, intent(in), optional :: x
        f = PRESENT(x)
    end function

end program
