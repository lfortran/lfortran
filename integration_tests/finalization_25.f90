! An allocated allocatable local of a finalizable type is finalized when it
! is deallocated at the end of its procedure or BLOCK construct (#13638).
module finalization_25_mod
    implicit none
    integer :: nfin = 0, last = 0
    type :: t
        integer :: c = 0
    contains
        final :: fin
    end type
contains
    subroutine fin(self)
        type(t), intent(inout) :: self
        nfin = nfin + 1
        last = self%c
    end subroutine
end module

program finalization_25
    use finalization_25_mod
    implicit none
    integer :: r

    call at_end()
    if (nfin /= 1 .or. last /= 4) error stop 1

    call never_allocated()
    if (nfin /= 1) error stop 2

    call early_return(.true.)
    if (nfin /= 2 .or. last /= 5) error stop 3
    call early_return(.false.)
    if (nfin /= 3 .or. last /= 6) error stop 4

    call explicitly_deallocated()
    if (nfin /= 4 .or. last /= 7) error stop 5

    r = func()
    if (r /= 8) error stop 6
    if (nfin /= 5 .or. last /= 8) error stop 7

    call in_block()
    if (nfin /= 6 .or. last /= 9) error stop 8

    block
        type(t), allocatable :: b
        allocate(b)
        b%c = 10
    end block
    if (nfin /= 7 .or. last /= 10) error stop 9
    print *, nfin, last
contains
    subroutine at_end()
        type(t), allocatable :: a
        allocate(a)
        a%c = 4
    end subroutine

    subroutine never_allocated()
        type(t), allocatable :: a
        if (allocated(a)) error stop 10
    end subroutine

    subroutine early_return(early)
        logical, intent(in) :: early
        type(t), allocatable :: a
        allocate(a)
        a%c = 5
        if (early) return
        a%c = 6
    end subroutine

    subroutine explicitly_deallocated()
        type(t), allocatable :: a
        allocate(a)
        a%c = 7
        deallocate(a)
        if (nfin /= 4) error stop 11
    end subroutine

    integer function func() result(res)
        type(t), allocatable :: a
        allocate(a)
        a%c = 8
        res = a%c
    end function

    subroutine in_block()
        block
            type(t), allocatable :: a
            allocate(a)
            a%c = 9
        end block
        if (nfin /= 6) error stop 12
    end subroutine
end program
