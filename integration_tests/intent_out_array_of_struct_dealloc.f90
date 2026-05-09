module intent_out_array_of_struct_dealloc_m
    implicit none

    type :: metric_dict_type
        character(10) :: key
        real :: val
        real, allocatable, dimension(:) :: history
    end type metric_dict_type
contains
    subroutine metric_dict_alloc(input, source)
        type(metric_dict_type), dimension(:), intent(out) :: input
        type(metric_dict_type), dimension(:), intent(in) :: source
        integer :: i
        do i = 1, size(input, dim=1)
            input(i)%key = source(i)%key
            allocate(input(i)%history(size(source(i)%history, dim=1)))
        end do
    end subroutine metric_dict_alloc
end module intent_out_array_of_struct_dealloc_m

program intent_out_array_of_struct_dealloc
    use intent_out_array_of_struct_dealloc_m, only: metric_dict_type, metric_dict_alloc
    implicit none

    type(metric_dict_type), allocatable, dimension(:) :: arr1, arr2
    integer :: i

    allocate(arr1(2))
    allocate(arr2(2))

    do i = 1, 2
        arr1(i)%key = "metric"
        arr1(i)%val = real(i)
        allocate(arr1(i)%history(3))
        arr1(i)%history = real(i)
    end do
    do i = 1, 2
        allocate(arr2(i)%history(5))
        arr2(i)%history = -1.0
    end do

    call metric_dict_alloc(arr2, source=arr1)

    do i = 1, 2
        if (.not. allocated(arr2(i)%history)) error stop 1
        if (size(arr2(i)%history) /= 3) error stop 2
        if (arr2(i)%key /= arr1(i)%key) error stop 3
    end do
end program intent_out_array_of_struct_dealloc
