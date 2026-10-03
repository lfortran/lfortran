! Tests that the cached finalizer of derived-type descriptor arrays reads
! the array size from the descriptor. The LLVM backend emits one finalizer
! per rank and element type and reuses it: here it is first emitted for the
! fixed-shape temporary `reshape(a, [4])` and then reused for allocatable
! arrays of other sizes.
module arrays_reshape_47_m
    implicit none
    type :: t
        integer, allocatable :: v(:)
    end type t
contains
    subroutine run_reshape(a)
        type(t), intent(in) :: a(:)
        call check(reshape(a, [4]))
    end subroutine

    subroutine check(arg)
        type(t), intent(in) :: arg(:)
        integer :: k
        if (size(arg) /= 4) error stop
        do k = 1, 4
            if (size(arg(k)%v) /= 3) error stop
            if (any(arg(k)%v /= k)) error stop
        end do
    end subroutine

    subroutine run_small(n)
        integer, intent(in) :: n
        type(t), allocatable :: b(:)
        integer, allocatable :: x(:)
        allocate(x(8))
        x = 12345
        deallocate(x)
        allocate(b(n))
        if (size(b) /= n) error stop
    end subroutine
end module

program arrays_reshape_47
    use arrays_reshape_47_m
    implicit none
    type(t) :: a(4)
    integer :: k
    do k = 1, 4
        allocate(a(k)%v(3))
        a(k)%v = k
    end do
    call run_reshape(a)
    call run_small(1)
    call run_small(0)
    print *, "ok"
end program
