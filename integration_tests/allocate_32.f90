program allocate_32
    implicit none
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

    print *, "Before: size =", size(items)
    do i = 1, size(items)
        print *, "  items(", i, ")%name =", trim(items(i)%name), &
                 ", value =", items(i)%value
    end do

    items = items(2:3)

    print *, "After: size =", size(items)
    do i = 1, size(items)
        print *, "  items(", i, ")%name =", trim(items(i)%name), &
                 ", value =", items(i)%value
    end do

    if (size(items) /= 2) error stop "Expected size 2"
    if (items(1)%value /= 20) error stop "Expected items(1)%value = 20"
    if (items(2)%value /= 30) error stop "Expected items(2)%value = 30"
    print *, "PASS"
end program allocate_32
