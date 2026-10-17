module pdt_25_mod
    ! Function results whose LEN type parameters are specification
    ! expressions of the dummy arguments.
    implicit none

    type :: matrix_t(nrow, ncol)
        integer, len :: nrow, ncol
        real :: x(nrow, ncol)
    end type

    type :: box_t(n)
        integer, len :: n
        character(len=n) :: text
    end type

contains

    function make_matrix(a) result(mat)
        real, intent(in) :: a(:, :)
        type(matrix_t(nrow=size(a, 1), ncol=size(a, 2))) :: mat
        mat%x = a
    end function

    function transpose_copy(this) result(out)
        type(matrix_t(*, *)), intent(in) :: this
        type(matrix_t(nrow=this%ncol, ncol=this%nrow)) :: out
        out%x = transpose(this%x)
    end function

    function make_box(s) result(box)
        character(len=*), intent(in) :: s
        type(box_t(n=len(s))) :: box
        box%text = s
    end function

end module

program pdt_25
    use pdt_25_mod
    implicit none
    type(matrix_t(2, 3)) :: a
    type(matrix_t(3, 2)) :: at
    type(box_t(5)) :: b
    real :: raw(2, 3)

    raw = reshape([1.0, 4.0, 2.0, 5.0, 3.0, 6.0], [2, 3])
    a = make_matrix(raw)
    if (a%nrow /= 2 .or. a%ncol /= 3) error stop
    if (abs(a%x(2, 3) - 6.0) > 1e-6) error stop
    at = transpose_copy(a)
    if (at%nrow /= 3 .or. at%ncol /= 2) error stop
    if (abs(at%x(3, 2) - 6.0) > 1e-6) error stop
    if (abs(at%x(1, 2) - 4.0) > 1e-6) error stop
    b = make_box("hello")
    if (b%n /= 5 .or. b%text /= "hello") error stop
    print *, sum(a%x), sum(at%x), b%text
end program
