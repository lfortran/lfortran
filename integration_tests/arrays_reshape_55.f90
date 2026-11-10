module arrays_reshape_55_mod
implicit none
contains
function thomas_fermi_potential(x, Z) result(y)
    real(8), intent(in) :: x(:)
    real(8), intent(in) :: Z
    real(8) :: y(size(x))

    y = Z / (x + 1.0d0)
end function

subroutine test_reshape()
    integer :: Nq, Ne, i, j
    real(8), allocatable :: xq(:,:), Vin(:,:)
    real(8) :: Z
    Nq = 10
    Ne = 5

    allocate(xq(Nq, Ne))
    allocate(Vin(Nq, Ne))

    do j = 1, Ne
        do i = 1, Nq
            xq(i, j) = i + 10*j
        end do
    end do
    Z = 2.0d0
    ! reshape of a function result whose argument is itself a reshape,
    ! followed by an array arithmetic operation
    Vin = reshape(thomas_fermi_potential(reshape(xq, [Nq*Ne]), Z), [Nq, Ne]) + Z / xq
    print *, "Vin(1,1) =", Vin(1,1)
    do j = 1, Ne
        do i = 1, Nq
            if (abs(Vin(i, j) - (Z/(xq(i, j) + 1.0d0) + Z/xq(i, j))) > 1.0d-12) error stop
        end do
    end do
end subroutine
end module arrays_reshape_55_mod

program arrays_reshape_55
    use arrays_reshape_55_mod
    implicit none
    call test_reshape()
end program arrays_reshape_55
