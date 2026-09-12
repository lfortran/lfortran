program doloop_20
! A DO loop-control expression only has to be a scalar-int-expr (F2018
! R1125), so a POINTER or ALLOCATABLE integer is perfectly valid there.
! LFortran used to emit a spurious "must be integer" warning for these,
! because it tested the declared type without looking past the Pointer /
! Allocatable wrapper.
implicit none
integer, pointer :: lo_ptr, hi_ptr, step_ptr
integer, target :: lo, hi, step
integer, allocatable :: hi_alloc
integer :: i, total

    lo = 1
    hi = 5
    step = 1
    lo_ptr => lo
    hi_ptr => hi
    step_ptr => step
    allocate(hi_alloc)
    hi_alloc = 5

!   end expression is a pointer
    total = 0
    do i = 1, hi_ptr
        total = total + i
    end do
    if (total /= 15) error stop

!   end expression is a parenthesized pointer
    total = 0
    do i = 1, (hi_ptr)
        total = total + i
    end do
    if (total /= 15) error stop

!   start expression is a pointer
    total = 0
    do i = lo_ptr, 5
        total = total + i
    end do
    if (total /= 15) error stop

!   step expression is a pointer
    total = 0
    do i = 1, 5, step_ptr
        total = total + i
    end do
    if (total /= 15) error stop

!   pointer inside a larger integer expression
    total = 0
    do i = 1, hi_ptr*2
        total = total + i
    end do
    if (total /= 55) error stop

!   end expression is an allocatable
    total = 0
    do i = 1, hi_alloc
        total = total + i
    end do
    if (total /= 15) error stop

    deallocate(hi_alloc)
end program doloop_20
