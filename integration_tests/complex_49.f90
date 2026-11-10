module complex_49_mod
    ! complex array minus complex scalar inside sum(..., dim) with a result
    ! extent computed by a pure function in the specification part (#3780)
    implicit none
contains

    pure function my_merge(val1, val2, mask) result(merged_value)
        integer, intent(in) :: val1, val2
        logical, intent(in) :: mask
        integer :: merged_value
        if (mask) then
            merged_value = val1
        else
            merged_value = val2
        end if
    end function my_merge

    function moment_scalar_2_csp_csp(x, order, dim, center, mask) result(res)
        complex, intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex, intent(in) :: center
        logical, intent(in), optional :: mask
        complex :: res(my_merge(size(x, 1), size(x, 2), mask=1<dim))

        res = sum((x - center), dim) / size(x, dim)
    end function moment_scalar_2_csp_csp

end module complex_49_mod

program complex_49
    use complex_49_mod, only: moment_scalar_2_csp_csp
    implicit none
    complex :: x(2, 3), center
    complex :: r1(3), r2(2)
    integer :: i, j

    do j = 1, 3
        do i = 1, 2
            x(i, j) = cmplx(real(i + 2*(j - 1)), real(10*i + j))
        end do
    end do
    center = (1.0, 2.0)

    if (size(moment_scalar_2_csp_csp(x, 1, 1, center)) /= 3) error stop
    r1 = moment_scalar_2_csp_csp(x, 1, 1, center)
    print *, r1
    do j = 1, 3
        if (abs(r1(j) - (sum(x(:, j)) - 2*center) / 2) > 1.0e-5) error stop
    end do
    if (abs(r1(1) - (0.5, 14.0)) > 1.0e-5) error stop

    if (size(moment_scalar_2_csp_csp(x, 1, 2, center)) /= 2) error stop
    r2 = moment_scalar_2_csp_csp(x, 1, 2, center)
    print *, r2
    do i = 1, 2
        if (abs(r2(i) - (sum(x(i, :)) - 3*center) / 3) > 1.0e-5) error stop
    end do
    if (abs(r2(1) - (2.0, 10.0)) > 1.0e-5) error stop
end program
