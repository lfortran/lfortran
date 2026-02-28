! This program caused a segfault due to memory leak in ReAlloc Node inside array_op pass
program arrays_109
    implicit none
    integer, parameter :: n = 100
    integer :: i, j
    integer,  dimension(:,:,:), allocatable :: perimeter
    do i = 1, n
        do j = 1, n
            if (allocated(perimeter)) deallocate(perimeter)
            call go(i)
            if (perimeter(1,i,j) /= i) error stop
            if (perimeter(2,i,j) /= i) error stop
            if (perimeter(3,i,j) /= i) error stop
            if (perimeter(4,i,j) /= i) error stop
        end do
    end do
    contains
    subroutine go(i)
        integer, intent(in) :: i
        if (.not. allocated(perimeter)) then
            allocate(perimeter(4, n, n)) 
            perimeter = i
        end if
    end subroutine go
end program arrays_109