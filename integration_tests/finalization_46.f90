! An intrinsic assignment to an allocatable scalar of a finalizable derived
! type allocates the variable when it is unallocated, and then does not
! finalize it (F2018 7.5.6.3 p1). An allocated variable is finalized once,
! before it is defined, whether the type is finalizable itself or only
! through a component. In both cases a component with a type-bound defined
! assignment is assigned by it (F2018 10.2.1.3 p13), and the value may be
! the variable.
module finalization_46_m
    implicit none
    integer :: nf = 0
    integer :: nfc = 0
    integer :: na = 0
    integer :: last = 0

    type :: asg_t
        integer :: k = 0
    contains
        procedure :: asg
        generic :: assignment(=) => asg
    end type

    type :: item
        integer :: v = 0
        type(asg_t) :: c
    contains
        final :: fin
    end type

    type :: comp_t
        integer :: k = 0
    contains
        final :: fin_comp
    end type

    type :: holder_t
        integer :: v = 0
        type(comp_t) :: d
    end type
contains
    subroutine fin(x)
        type(item), intent(inout) :: x
        nf = nf + 1
        last = x%v
    end subroutine

    subroutine fin_comp(x)
        type(comp_t), intent(inout) :: x
        nfc = nfc + 1
        last = x%k
    end subroutine

    subroutine asg(a, b)
        class(asg_t), intent(out) :: a
        type(asg_t), intent(in) :: b
        na = na + 1
        a%k = b%k + 100
    end subroutine
end module

program finalization_46
    use finalization_46_m
    implicit none
    type(item), allocatable :: a
    type(item) :: b
    type(holder_t), allocatable :: h
    type(holder_t) :: g

    b%v = 1
    b%c%k = 1
    a = b
    if (nf /= 0) error stop 1
    if (na /= 1) error stop 2
    if (.not. allocated(a)) error stop 3
    if (a%v /= 1 .or. a%c%k /= 101) error stop 4

    b%v = 2
    b%c%k = 2
    a = b
    if (nf /= 1 .or. last /= 1) error stop 5
    if (na /= 2) error stop 6
    if (a%v /= 2 .or. a%c%k /= 102) error stop 7

    deallocate(a)
    nf = 0
    na = 0
    b%v = 3
    a = b
    if (nf /= 0 .or. na /= 1) error stop 10
    if (a%v /= 3) error stop 11
    deallocate(a)

    g%v = 4
    g%d%k = 4
    h = g
    if (nfc /= 0) error stop 12
    if (h%v /= 4 .or. h%d%k /= 4) error stop 13
    g%v = 5
    g%d%k = 5
    h = g
    if (nfc /= 1 .or. last /= 4) error stop 14
    if (h%v /= 5 .or. h%d%k /= 5) error stop 15

    ! The value is the variable.
    h = h
    if (nfc /= 2 .or. last /= 5) error stop 16
    if (h%v /= 5 .or. h%d%k /= 5) error stop 17
    print *, "ok"
end program
