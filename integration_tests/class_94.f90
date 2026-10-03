! Tests passing struct arrays to class array parameters
module class_94_types
    implicit none

    type :: base_type
        integer :: value
    end type base_type

    type, extends(base_type) :: extended_type
        integer :: extra
    end type extended_type

contains

    ! Subroutine that accepts class array
    subroutine process_class_array(arr)
        class(base_type), intent(in) :: arr(:)
        integer :: i

        do i = 1, size(arr)
             select type(arr)
             type is (base_type)
                if (arr(i)%value /= i * 10) error stop "Base type value mismatch"
             type is (extended_type)
                 if (arr(i)%value /= i * 10) error stop "Extended type value mismatch"
                 if (arr(i)%extra /= i * 100) error stop "Extended type extra mismatch"
             end select
        end do
    end subroutine process_class_array

    ! Subroutine that accepts class array and modifies it
    subroutine modify_class_array(arr)
        class(base_type), intent(inout) :: arr(:)
        integer :: i

        do i = 1, size(arr)
             select type(arr)
             type is (base_type)
                arr(i)%value = arr(i)%value + 1
             type is (extended_type)
                 arr(i)%value = arr(i)%value + 1
                 arr(i)%extra = arr(i)%extra + 1
             end select
        end do
    end subroutine modify_class_array

    ! Function that accepts class array and returns sum
    function sum_class_values(arr) result(total)
        class(base_type), intent(in) :: arr(:)
        integer :: total, i

        total = 0
        do i = 1, size(arr)
            total = total + arr(i)%value
        end do
    end function sum_class_values

end module class_94_types

program class_94
    use class_94_types
    implicit none

    type(base_type), allocatable :: base_arr(:)
    type(extended_type), allocatable :: ext_arr(:)
    integer :: i, total

    ! Test 1: Pass base_type array to class array parameter
    print *, "Test 1: base_type array -> class array"
    allocate(base_arr(3))
    do i = 1, 3
        base_arr(i)%value = i * 10
    end do
    call process_class_array(base_arr)
    print *, "Test 1 passed"

    !Test 2: Pass extended_type array to class array parameter
     print *, "Test 2: extended_type array -> class array"
     allocate(ext_arr(3))
     do i = 1, 3
         ext_arr(i)%value = i * 10
         ext_arr(i)%extra = i * 100
     end do
     call process_class_array(ext_arr)
     print *, "Test 2 passed"

    ! ! Test 3: Modify base_type array via class array parameter
    print *, "Test 3: Modify base_type array"
    call modify_class_array(base_arr)
    do i = 1, 3
        if (base_arr(i)%value /= i * 10 + 1) error stop "Modification failed"
    end do
    print *, "Test 3 passed"

    !Test 4: Modify extended_type array via class array parameter
     print *, "Test 4: Modify extended_type array"
     call modify_class_array(ext_arr)
     do i = 1, 3
         if (ext_arr(i)%value /= i * 10 + 1) error stop "Extended modification failed"
         if (ext_arr(i)%extra /= i * 100 + 1) error stop "Extended extra modification failed"
     end do
     print *, "Test 4 passed"

    ! Test 5: Use function with class array parameter
    print *, "Test 5: Function with class array"
    total = sum_class_values(base_arr)
    if (total /= 63) error stop "Sum of base_arr failed"  ! (11 + 21 + 31)
    
    total = sum_class_values(ext_arr)
    if (total /= 63) error stop "Sum of ext_arr failed"   ! (11 + 21 + 31)
    print *, "Test 5 passed"

    ! Test 6: Same type as class (base_type -> class(base_type))
    print *, "Test 6: Same type as class"
    deallocate(base_arr)
    allocate(base_arr(2))
    base_arr(1)%value = 100
    base_arr(2)%value = 200
    total = sum_class_values(base_arr)
    print *, "Total:", total
    if (total /= 300) error stop "Same type test failed"
    print *, "Test 6 passed"
end program class_94
