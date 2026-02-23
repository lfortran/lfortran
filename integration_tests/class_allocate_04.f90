program class_allocate_04
    ! Test allocate with type-spec for class(*) arrays
    implicit none
    class(*), allocatable :: arr(:)
    integer :: i

    allocate(integer :: arr(5))
    select type(arr)
        type is (integer)
            do i = 1, 5
                arr(i) = i * 10
            end do
            do i = 1, 5
                if (arr(i) /= i * 10) error stop
            end do
        class default
            error stop
    end select

    print *, "PASS"
end program class_allocate_04
