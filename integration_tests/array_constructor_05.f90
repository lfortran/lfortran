program array_constructor_05
    implicit none
    type :: t
        integer :: cells_
    end type
    type(t) :: self
    integer :: k, c
    self%cells_ = 3
    associate (b => [-1D0, [(0D0, c = 1, self%cells_)], 1D0])
        if (size(b) /= self%cells_ + 2) error stop 1
        if (abs(b(1) + 1D0) > 1D-15) error stop 2
        if (abs(b(size(b)) - 1D0) > 1D-15) error stop 3
        do k = 2, size(b) - 1
            if (abs(b(k)) > 1D-15) error stop 4
        end do
    end associate
end program array_constructor_05
