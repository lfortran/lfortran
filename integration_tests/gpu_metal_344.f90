module gpu_metal_344_m
    implicit none

    type :: operator_t
        integer :: m_
    end type

contains

    pure function matrix_multiply(self, vec) result(r)
        class(operator_t), intent(in) :: self
        real, intent(in) :: vec(:)
        real, allocatable :: r(:)
        integer :: row
        allocate(r(size(vec)))
        do concurrent(row = 1:size(r))
            r(row) = real(self%m_) * dot_product(vec(row:row), vec(row:row))
        end do
    end function

    pure function e(dir, length) result(unit_vector)
        integer, intent(in) :: dir, length
        real :: unit_vector(length)
        unit_vector = 0.0
        unit_vector(dir) = 1.0
    end function

    subroutine assemble(self, G)
        type(operator_t), intent(in) :: self
        real, intent(out) :: G(:,:)
        integer :: col
        associate(cols => self%m_ + 2)
            do concurrent(col = 1:cols)
                G(:,col) = matrix_multiply(self, e(dir=col, length=cols))
            end do
        end associate
    end subroutine

end module

program gpu_metal_344
    use gpu_metal_344_m
    implicit none
    type(operator_t) :: op
    real, allocatable :: G(:,:)
    integer :: i, j, n

    op%m_ = 4
    n = op%m_ + 2
    allocate(G(n,n))
    call assemble(op, G)
    do j = 1, n
        do i = 1, n
            if (i == j) then
                if (abs(G(i,j) - 4.0) > 1e-6) error stop
            else
                if (abs(G(i,j)) > 1e-6) error stop
            end if
        end do
    end do
    print *, sum(G)
    if (abs(sum(G) - 24.0) > 1e-5) error stop
end program
