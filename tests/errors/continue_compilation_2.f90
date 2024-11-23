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

program continue_compilation_2
    use iso_fortran_env
    use iso_c_binding, only: c_ptr, c_f_pointer
    use Geometry
    implicit none

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
    !array_constructor_with_different_kind
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
    integer :: idaa2_x(1:2,1:2,1:2)
    integer :: idaa2_y(1:2,1:2,1:1)
    idaa2_x = reshape([1, 2, 3, 4, 5, 6, 7, 8], [2, 2, 2])
    idaa2_y = reshape([1, 2, 3, 4], [2, 2, 1])
    idaa2_y = idaa2_x
    !incompatible_dimension_logical_arrays_logical_binop_01
    logical, parameter :: idlalb1_x(3) = [.true., .false., .false.]
    print *, idlalb1_x .neqv. [.true., .true.]
    !incompatible_dimension_logical_arrays_logical_binop_02
    logical, parameter :: idlalb2_x1(3) = [.true., .false., .false.]
    logical, parameter :: idlalb2_x2(2) = [.true., .true.]
    print *, idlalb2_x1 .neqv. idlalb2_x2
    !incompatible_rank_allocatable_arr1
    integer, allocatable :: iraa1_arr1(:, :)
    iraa1_arr1 = [1, 2, 3]
    !incompatible_rank_allocatable_arr2
    integer, allocatable :: iraa2_arr1(:, :, :)
    integer, allocatable :: iraa2_arr3(:)
    iraa2_arr3 = iraa2_arr1

    CONTAINS
    LOGICAL FUNCTION f(x)
        INTEGER, INTENT(IN), OPTIONAL :: x
        f = PRESENT(x)
    END FUNCTION

end program
