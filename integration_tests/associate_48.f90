module associate_48_mod
    implicit none
    type :: elem_t
        integer, allocatable :: v
    end type
    type :: outer_t
        type(elem_t), allocatable :: items(:)
    end type
    type :: ptr_t
        type(outer_t), pointer :: ptr => null()
    end type
contains
    subroutine sub(a, b)
        type(outer_t), intent(inout) :: a
        type(outer_t), intent(in) :: b
        integer :: i
        do i = 1, size(a%items)
            a%items(i)%v = a%items(i)%v + b%items(i)%v
        end do
    end subroutine
    subroutine process(arr, n)
        type(ptr_t), intent(inout) :: arr(:)
        integer, intent(in) :: n
        associate(x => arr(1)%ptr)
            if (n > 0) call sub(x, arr(1)%ptr)
        end associate
    end subroutine
end module

program associate_48
    use associate_48_mod
    implicit none
    type(ptr_t) :: arr(1)
    allocate(arr(1)%ptr)
    allocate(arr(1)%ptr%items(2))
    arr(1)%ptr%items(1)%v = 3
    arr(1)%ptr%items(2)%v = 7
    call process(arr, 1)
    if (arr(1)%ptr%items(1)%v /= 6) error stop
    if (arr(1)%ptr%items(2)%v /= 14) error stop
    print *, "done"
end program
