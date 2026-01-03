! Test self-referencing array section assignments with --realloc-lhs-arrays
! This tests the fix for use-after-free when arr = arr(i:j) causes realloc
! to free/move memory before the copy completes.
program array_section_08
    implicit none

    call test_simple_array()
    call test_struct_array()

    print *, "PASS"

contains

    subroutine test_simple_array()
        real, allocatable :: arr(:)
        integer :: i

        allocate(arr(5))
        do i = 1, 5
            arr(i) = real(i * 10)
        end do

        arr = arr(3:5)  ! Self-referencing - was causing use-after-free

        if (size(arr) /= 3) error stop "simple: size should be 3"
        if (arr(1) /= 30.0) error stop "simple: arr(1) should be 30"
        if (arr(2) /= 40.0) error stop "simple: arr(2) should be 40"
        if (arr(3) /= 50.0) error stop "simple: arr(3) should be 50"
    end subroutine

    subroutine test_struct_array()
        type :: item_t
            character(len=:), allocatable :: name
            integer :: value
        end type item_t

        type(item_t), allocatable :: items(:)
        integer :: i

        allocate(items(3))
        do i = 1, 3
            allocate(character(len=10) :: items(i)%name)
            items(i)%name = "item" // char(48 + i)
            items(i)%value = i * 10
        end do

        items = items(2:3)  ! Self-referencing struct array

        if (size(items) /= 2) error stop "struct: size should be 2"
        if (items(1)%value /= 20) error stop "struct: items(1)%value should be 20"
        if (items(2)%value /= 30) error stop "struct: items(2)%value should be 30"
    end subroutine

end program array_section_08
