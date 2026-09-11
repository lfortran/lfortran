program derived_types_161
    implicit none

    type :: pair_t
        integer :: x, y
    end type pair_t

    type(pair_t), allocatable :: mat(:,:)
    type(pair_t) :: fixed(3,3)
    integer :: i, j

    allocate(mat(5,5))
    mat = pair_t(-999, -999)

    ! scalar structure constructor broadcast over an array section
    mat(1,:) = pair_t(1, 100)
    do j = 1, 5
        if (mat(1,j)%x /= 1) error stop
        if (mat(1,j)%y /= 100) error stop
    end do
    do i = 2, 5
        do j = 1, 5
            if (mat(i,j)%x /= -999) error stop
            if (mat(i,j)%y /= -999) error stop
        end do
    end do

    ! strided section
    mat(:,2) = pair_t(7, 8)
    do i = 1, 5
        if (mat(i,2)%x /= 7) error stop
        if (mat(i,2)%y /= 8) error stop
    end do
    if (mat(1,1)%x /= 1) error stop
    if (mat(1,3)%x /= 1) error stop

    ! section with an explicit stride
    mat(3, 1:5:2) = pair_t(11, 22)
    do j = 1, 5, 2
        if (mat(3,j)%x /= 11) error stop
        if (mat(3,j)%y /= 22) error stop
    end do
    if (mat(3,4)%x /= -999) error stop

    ! non-allocatable array, whole array and section
    fixed = pair_t(-1, -2)
    do i = 1, 3
        do j = 1, 3
            if (fixed(i,j)%x /= -1) error stop
            if (fixed(i,j)%y /= -2) error stop
        end do
    end do
    fixed(2,:) = pair_t(5, 6)
    do j = 1, 3
        if (fixed(2,j)%x /= 5) error stop
        if (fixed(2,j)%y /= 6) error stop
    end do
    if (fixed(1,1)%x /= -1) error stop

    print *, mat(1,1)%x, mat(1,1)%y
end program derived_types_161
