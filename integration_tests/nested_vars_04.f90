module m_sellc
    implicit none

    integer, parameter :: sp = kind(1.0)
    integer, parameter :: ilp = kind(1)

    type :: SELLC_type
        integer(ilp), allocatable :: rowptr(:)
        integer(ilp), allocatable :: col(:,:)
    end type

    type, extends(SELLC_type) :: SELLC_sp_type
        real(sp), allocatable :: data(:,:)
    end type

contains

    subroutine spmv_sellc_sp(matrix, vec_x, vec_y)
        type(SELLC_sp_type), intent(in) :: matrix
        real(sp), intent(in) :: vec_x(:)
        real(sp), intent(inout) :: vec_y(:)

        integer :: i, nz, rowidx
        real(sp) :: alpha_

        alpha_ = 1.0_sp
        i = 1
        rowidx = 1
        nz = matrix%rowptr(i + 1) - matrix%rowptr(i)

        associate(data => matrix%data, ia => matrix%rowptr, ja => matrix%col)
            call chunk_kernel_4(nz, data(:, ia(i)), ja(:, ia(i)), vec_x, vec_y(rowidx:))
        end associate

    contains

        subroutine chunk_kernel_4(n, a, col, x, y)
            integer, intent(in) :: n
            real(sp), intent(in) :: a(4)
            integer(ilp), intent(in) :: col(4)
            real(sp), intent(in) :: x(:)
            real(sp), intent(inout) :: y(4)

            integer :: j

            do j = 1, n
                y(:) = y(:) + alpha_ * a(:) * x(col(:))
            end do
        end subroutine chunk_kernel_4

    end subroutine spmv_sellc_sp

end module m_sellc

program test_sellc
    use m_sellc
    implicit none

    type(SELLC_sp_type) :: matrix
    real(sp) :: vec_x(6), vec_y(4)
    real(sp) :: expected(4)

    allocate(matrix%data(4, 7))
    allocate(matrix%col(4, 7))
    allocate(matrix%rowptr(3))

    matrix%data = reshape([ &
        1, 2, 3, 0, 13, 14, 0, &
        4, 5, 6, 7, 15, 16, 17, &
        8, 9, 10, 0, 0, 0, 0, &
        11, 12, 0, 0, 0, 0, 0 ], shape(matrix%data))

    matrix%col = reshape([ &
        1, 3, 4, 1, 4, 5, 1, &
        2, 3, 5, 6, 2, 5, 6, &
        1, 2, 3, 1, 1, 1, 1, &
        5, 6, 1, 1, 1, 1, 1 ], shape(matrix%col))

    matrix%rowptr = [1, 5, 8]

    vec_x = 1.0_sp
    vec_y = 0.0_sp

    call spmv_sellc_sp(matrix, vec_x, vec_y)

    expected = [4.0_sp, 8.0_sp, 12.0_sp, 0.0_sp]
    if (any(abs(vec_y - expected) > 1.0e-6_sp)) then
        error stop "nested_vars_04: unexpected vec_y"
    end if

    print *, "vec_y =", vec_y
end program test_sellc