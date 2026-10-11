! An allocated allocatable scalar component is finalized when the structure
! that it is a component of is finalized or deallocated: at the end of the
! procedure for a local, or by DEALLOCATE of an allocatable structure.
module finalization_39_mod
    implicit none
    integer :: nfin = 0, total = 0
    integer :: order(4) = 0
    type :: t
        integer :: v = 0
    contains
        final :: fin_t
    end type
    type, extends(t) :: d
    contains
        final :: fin_d
    end type
    type :: holder
        type(t), allocatable :: c
    end type
    type :: dholder
        type(d), allocatable :: c
    end type
    type :: outer
        type(holder) :: h
    end type
contains
    subroutine fin_t(x)
        type(t), intent(inout) :: x
        nfin = nfin + 1
        total = total + x%v
        order(nfin) = 1
    end subroutine

    subroutine fin_d(x)
        type(d), intent(inout) :: x
        nfin = nfin + 1
        total = total + 10*x%v
        order(nfin) = 2
    end subroutine

    subroutine local_component()
        type(holder) :: h
        allocate(h%c)
        h%c%v = 4
    end subroutine

    subroutine unallocated_component()
        type(holder) :: h
    end subroutine

    subroutine deallocated_component()
        type(holder) :: h
        allocate(h%c)
        h%c%v = 5
        deallocate(h%c)
    end subroutine

    subroutine deallocate_structure()
        type(holder), allocatable :: h
        allocate(h)
        allocate(h%c)
        h%c%v = 6
        deallocate(h)
    end subroutine

    subroutine extended_component()
        type(dholder) :: h
        allocate(h%c)
        h%c%v = 7
    end subroutine

    subroutine nested_component()
        type(outer) :: o
        allocate(o%h%c)
        o%h%c%v = 8
    end subroutine
end module

program finalization_39
    use finalization_39_mod
    implicit none
    call local_component()
    if (nfin /= 1 .or. total /= 4) error stop 1
    call unallocated_component()
    if (nfin /= 1) error stop 2
    call deallocated_component()
    if (nfin /= 2 .or. total /= 9) error stop 3
    call deallocate_structure()
    if (nfin /= 3 .or. total /= 15) error stop 4
    nfin = 0
    total = 0
    call extended_component()
    if (nfin /= 2 .or. total /= 77) error stop 5
    if (order(1) /= 2 .or. order(2) /= 1) error stop 6
    call nested_component()
    if (nfin /= 3 .or. total /= 85) error stop 7
    print *, "ok"
end program
