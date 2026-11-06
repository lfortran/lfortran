module gpu_identifier_extents
    implicit none
    type :: class
        real, allocatable :: float(:,:)
        integer :: float_gpu
    end type
end module

program gpu_identifiers_03
    use gpu_identifier_extents
    implicit none
    type(class), allocatable :: records(:)
    real :: result(3,2)
    integer :: selected, i, j

    selected = 2
    allocate(records(2))
    allocate(records(1)%float(1,1), records(2)%float(-2:0,4:5))
    records(1)%float = -1.0
    records(2)%float = 7.0
    records(2)%float_gpu = 19
    result = 0.0
    do concurrent (j = 1:2)
        block
            block
                real :: work(size(records(selected)%float,1), size(records(selected)%float,2))
                integer :: k
                work = real(j)
                do k = 1, size(work,1)
                    result(k,j) = work(k,size(work,2)) + real(k)
                end do
            end block
        end block
    end do
    do j = 1, 2
        do i = 1, 3
            if (result(i,j) /= real(i+j)) error stop 1
        end do
    end do
    if (any(records(1)%float /= -1.0)) error stop 2
    if (any(records(2)%float /= 7.0)) error stop 3
    if (records(2)%float_gpu /= 19) error stop 4
    if (lbound(records(2)%float,1) /= -2) error stop 5
    if (lbound(records(2)%float,2) /= 4) error stop 6
    print *, "host component extents ok"
end program
