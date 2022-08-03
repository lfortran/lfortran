program arrays_op_9
implicit none

call f()

contains

function modify(n, array_a)  result(r)
    integer, intent(in) :: n
    real(4), intent(in) :: array_a(n)
    real(4) :: r(n)
    r = sqrt(array_a)
end function

subroutine verify(array_a, array_b, result, size)
    real(4), intent(in) :: array_a(:), array_b(:), result(:)
    integer, intent(in) :: size
    integer :: i
    real(4) :: eps
    eps = 1e-6

    do i = 1, size
        if ( abs(array_a(i) * array_a(i) + sqrt(array_b(i)) - result(i)) > eps )  error stop
    end do

end subroutine

subroutine f()
    integer :: i, j

    real(4) :: array_a(256), array_b(256), array_c(256)

    do i = 1, 256
        array_a(i) = i
    end do

    do j = 1, 256
        array_b(j) = j + 5
    end do

    array_c = array_a**2 + modify(256, array_b)
    call verify(array_a, array_b, array_c, 256)

end subroutine

end program
