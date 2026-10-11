! An allocatable array local, or an allocatable array component of a local,
! is finalized at the end of the procedure only if it is still allocated:
! never allocated, explicitly deallocated, or deallocated on entry to a
! procedure with an INTENT(OUT) dummy.
module finalization_38_mod
    implicit none
    integer :: nfin = 0, total = 0
    type :: t
        integer :: v = 0
    contains
        final :: fin_t1
    end type
    type, extends(t) :: d
        real :: r = 0
    end type
    type :: holder
        type(d), allocatable :: arr(:)
    end type
contains
    subroutine fin_t1(x)
        type(t), intent(inout) :: x(:)
        nfin = nfin + 1
        total = total + sum(x%v)
    end subroutine

    subroutine unallocated_base()
        type(t), allocatable :: b(:)
    end subroutine

    subroutine unallocated_extended()
        type(d), allocatable :: b(:)
    end subroutine

    subroutine deallocated_extended()
        type(d), allocatable :: b(:)
        allocate(b(2))
        b%v = [1, 2]
        deallocate(b)
    end subroutine

    subroutine unallocated_component()
        type(holder) :: h
    end subroutine

    subroutine deallocated_component()
        type(holder) :: h
        allocate(h%arr(2))
        h%arr%v = [3, 4]
        deallocate(h%arr)
    end subroutine

    subroutine intent_out_component()
        type(holder) :: h
        allocate(h%arr(2))
        h%arr%v = [5, 6]
        call reset_holder(h)
    end subroutine

    subroutine reset_holder(x)
        type(holder), intent(out) :: x
        if (allocated(x%arr)) error stop 20
    end subroutine
end module

program finalization_38
    use finalization_38_mod
    implicit none
    call unallocated_base()
    call unallocated_extended()
    if (nfin /= 0) error stop 1
    call deallocated_extended()
    if (nfin /= 1 .or. total /= 3) error stop 2
    call unallocated_component()
    if (nfin /= 1) error stop 3
    call deallocated_component()
    if (nfin /= 2 .or. total /= 10) error stop 4
    call intent_out_component()
    if (nfin /= 3 .or. total /= 21) error stop 5
    print *, "ok"
end program
