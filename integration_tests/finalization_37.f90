! The parent component of an array of an extended type is an array of the
! parent type (F2018 7.5.6.2 step 3), so it is finalized by the final
! subroutine of the parent type whose dummy argument has the rank of the
! array, rather than element by element. The array is finalized when it is
! deallocated, when it is an allocatable or nonallocatable local variable of
! a procedure that returns, and when it is an allocatable component of a
! structure that is deallocated.
module finalization_37_mod
    implicit none
    integer :: nfin0 = 0, nfin1 = 0, nfin2 = 0, nfin_e = 0
    integer :: total = 0
    type :: t
        integer :: v = 0
    contains
        final :: fin_t0
        final :: fin_t1
        final :: fin_t2
    end type
    type, extends(t) :: e
        integer :: w = 0
    contains
        final :: fin_e1
    end type
    type, extends(t) :: d
        real :: r = 0
    end type
    type :: holder
        type(d), allocatable :: arr(:)
    end type
contains
    subroutine fin_t0(x)
        type(t), intent(inout) :: x
        nfin0 = nfin0 + 1
    end subroutine

    subroutine fin_t1(x)
        type(t), intent(inout) :: x(:)
        nfin1 = nfin1 + 1
        total = total + sum(x%v)
        x%v = -1
    end subroutine

    subroutine fin_t2(x)
        type(t), intent(inout) :: x(:, :)
        nfin2 = nfin2 + 1
        total = total + sum(x%v)
        if (size(x, 1) /= 2 .or. size(x, 2) /= 3) error stop 20
    end subroutine

    subroutine fin_e1(x)
        type(e), intent(inout) :: x(:)
        ! The final subroutine of `e` comes before those of its parent type.
        if (nfin1 /= 0) error stop 21
        nfin_e = nfin_e + 1
        if (any(x%w /= 7)) error stop 22
    end subroutine

    subroutine reset()
        nfin0 = 0; nfin1 = 0; nfin2 = 0; nfin_e = 0; total = 0
    end subroutine

    subroutine local_arrays()
        type(d) :: a(3)
        type(d), allocatable :: b(:)
        a%v = [1, 2, 3]
        allocate(b(2))
        b%v = [10, 20]
    end subroutine
end module

program finalization_37
    use finalization_37_mod
    implicit none
    type(d), allocatable :: s, a(:), m(:, :)
    type(e), allocatable :: ea(:)
    type(holder), allocatable :: h

    allocate(s)
    s%v = 5
    deallocate(s)
    if (nfin0 /= 1 .or. nfin1 /= 0 .or. nfin2 /= 0) error stop 1
    call reset()

    allocate(a(4))
    a%v = [1, 2, 3, 4]
    a%r = 1.5
    deallocate(a)
    if (nfin0 /= 0 .or. nfin1 /= 1 .or. nfin2 /= 0) error stop 2
    if (total /= 10) error stop 3
    call reset()

    allocate(m(2, 3))
    m%v = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    deallocate(m)
    if (nfin0 /= 0 .or. nfin1 /= 0 .or. nfin2 /= 1) error stop 4
    if (total /= 21) error stop 5
    call reset()

    allocate(ea(3))
    ea%v = [1, 1, 1]
    ea%w = 7
    deallocate(ea)
    if (nfin_e /= 1 .or. nfin1 /= 1 .or. nfin0 /= 0) error stop 6
    if (total /= 3) error stop 7
    call reset()

    call local_arrays()
    if (nfin0 /= 0 .or. nfin1 /= 2 .or. nfin2 /= 0) error stop 8
    if (total /= 36) error stop 9
    call reset()

    allocate(h)
    allocate(h%arr(2))
    h%arr%v = [3, 4]
    deallocate(h)
    if (nfin0 /= 0 .or. nfin1 /= 1 .or. nfin2 /= 0) error stop 10
    if (total /= 7) error stop 11
    print *, "ok"
end program
