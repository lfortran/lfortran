program procedure_pointer_abstract_02
    use iso_fortran_env, only: real32
    implicit none

    abstract interface
        pure function compute_func(x, y) result(output)
            import :: real32
            real(real32), intent(in) :: x(:,:), y(:,:)
            real(real32), dimension(size(x,2)) :: output
        end function
    end interface

    procedure(compute_func), pointer :: my_func => null()

    real(real32) :: a(3, 4), b(3, 4), res(4)
    integer :: i

    a = 1.0_real32
    b = 2.0_real32
    my_func => my_compute

    res = my_func(a, b)

    do i = 1, 4
        if (abs(res(i) - 6.0_real32) > 1.0e-6_real32) error stop
    end do
    print *, "PASS"

contains
    pure function my_compute(x, y) result(output)
        real(real32), intent(in) :: x(:,:), y(:,:)
        real(real32), dimension(size(x,2)) :: output
        integer :: j
        do j = 1, size(x, 2)
            output(j) = sum(x(:,j) * y(:,j))
        end do
    end function
end program
