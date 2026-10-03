module realloc_lhs_25_mod
    type :: array_t
        real, allocatable :: val(:,:)
    end type array_t

    type :: base_t
        class(array_t), allocatable :: output(:,:)
    end type base_t
end module realloc_lhs_25_mod

program realloc_lhs_25
    use realloc_lhs_25_mod
    implicit none
    type(base_t) :: tmp

    allocate(array_t :: tmp%output(1,1))
    tmp%output(1,1)%val = reshape([1.0, 2.0, 3.0], [1,3])

    if (size(tmp%output(1,1)%val, 1) /= 1) error stop
    if (size(tmp%output(1,1)%val, 2) /= 3) error stop
    if (any(abs(tmp%output(1,1)%val(1,:) - [1.0, 2.0, 3.0]) > 1.0e-6)) error stop
end program realloc_lhs_25