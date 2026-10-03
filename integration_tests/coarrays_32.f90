program coarrays_32
    implicit none

    real, allocatable :: prototype(:,:)
    real, allocatable :: target_arr(:,:)[:]

    allocate(prototype(3, 4))

    allocate(target_arr[*], mold=prototype)

    if (.not. allocated(target_arr)) error stop "Allocation failed"

    deallocate(target_arr)
    deallocate(prototype)

end program coarrays_32