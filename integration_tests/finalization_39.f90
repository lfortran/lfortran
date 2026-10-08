! Intrinsic assignment of a structure finalizes the old value of its array
! components as arrays: by the final subroutine of matching rank, or an
! elemental one element by element. A nonelemental scalar final subroutine
! does not apply, and the new value is not finalized.
module finalization_39_mod
    implicit none
    integer :: n1 = 0, s1 = 0, n2 = 0, s2 = 0, ns = 0, ne = 0, se = 0
    type :: t
        integer :: v = 0
    contains
        final :: fin_t1
        final :: fin_t2
    end type
    type :: ts
        integer :: v = 0
    contains
        final :: fin_ts
    end type
    type :: te
        integer :: v = 0
    contains
        final :: fin_te
    end type
    type :: holder
        type(t), allocatable :: arr(:)
        type(t), allocatable :: mat(:,:)
        type(ts), allocatable :: sarr(:)
        type(te), allocatable :: earr(:)
        type(t) :: fixed(2)
    end type
contains
    subroutine fin_t1(x)
        type(t), intent(inout) :: x(:)
        n1 = n1 + 1
        s1 = s1 + sum(x%v)
    end subroutine

    subroutine fin_t2(x)
        type(t), intent(inout) :: x(:,:)
        n2 = n2 + 1
        s2 = s2 + sum(x%v)
    end subroutine

    subroutine fin_ts(x)
        type(ts), intent(inout) :: x
        ns = ns + 1
    end subroutine

    impure elemental subroutine fin_te(x)
        type(te), intent(inout) :: x
        ne = ne + 1
        se = se + x%v
    end subroutine

    subroutine run()
        type(holder) :: h, h2
        allocate(h%arr(2), h%mat(1, 2), h%sarr(2), h%earr(2))
        h%arr%v = [2, 3]
        h%mat%v = reshape([7, 8], [1, 2])
        h%sarr%v = [1, 1]
        h%earr%v = [4, 5]
        h%fixed%v = [10, 20]
        allocate(h2%arr(1), h2%sarr(1), h2%earr(3))
        h2%arr%v = [5]
        h2%sarr%v = [6]
        h2%earr%v = [1, 2, 3]
        h2%fixed%v = [30, 40]
        n1 = 0; s1 = 0; n2 = 0; s2 = 0; ns = 0; ne = 0; se = 0
        h = h2
        ! h%arr and h%fixed by fin_t1, h%mat (h2%mat is unallocated) by fin_t2
        if (n1 /= 2) error stop 1
        if (s1 /= 2 + 3 + 10 + 20) error stop 2
        if (n2 /= 1) error stop 3
        if (s2 /= 7 + 8) error stop 4
        if (ns /= 0) error stop 5
        if (ne /= 2) error stop 6
        if (se /= 4 + 5) error stop 7
        if (allocated(h%mat)) error stop 8
        if (size(h%arr) /= 1) error stop 9
        if (h%arr(1)%v /= 5) error stop 10
        if (size(h%earr) /= 3) error stop 11
        if (any(h%earr%v /= [1, 2, 3])) error stop 12
        if (any(h%fixed%v /= [30, 40])) error stop 13
    end subroutine
end module

program finalization_39
    use finalization_39_mod
    implicit none
    call run()
    print *, "ok"
end program
