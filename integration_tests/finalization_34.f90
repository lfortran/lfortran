! DEALLOCATE of an allocatable of an extended type finalizes the parent
! component (F2018 7.5.6.3 p2, 7.5.6.2 step 3), for a scalar and for an array,
! with and without a final subroutine of the extended type itself. For an
! array, the final subroutines of a type are called for the whole array before
! those of its parent type. A polymorphic entity is finalized once, as its
! dynamic type.
module finalization_34_mod
    implicit none
    character(len=64) :: log = ""
    integer :: nfin = 0
    type :: t
        integer :: v = 0
    contains
        final :: fin_t
    end type
    type, extends(t) :: e
        integer :: w = 0
    end type
    type, extends(e) :: f
    contains
        final :: fin_f
    end type
    type, extends(f) :: g
        integer :: z = 0
    end type
contains
    impure elemental subroutine fin_t(x)
        type(t), intent(inout) :: x
        nfin = nfin + 1
        log = trim(log) // "T" // achar(48 + x%v)
    end subroutine

    impure elemental subroutine fin_f(x)
        type(f), intent(inout) :: x
        log = trim(log) // "F" // achar(48 + x%v)
    end subroutine

    subroutine check(expected, code)
        character(len=*), intent(in) :: expected
        integer, intent(in) :: code
        print *, trim(log)
        if (trim(log) /= expected) error stop code
        log = ""
    end subroutine
end module

program finalization_34
    use finalization_34_mod
    implicit none
    type(e), allocatable :: se, ae(:)
    type(f), allocatable :: sf, af(:)
    type(g), allocatable :: sg, ag(:, :)
    class(t), allocatable :: ct

    ! The reproducer of the issue: no final subroutine of `e` itself.
    allocate(se)
    se%v = 1
    deallocate(se)
    if (nfin /= 1) error stop 1
    allocate(ae(2))
    ae%v = [2, 3]
    deallocate(ae)
    if (nfin /= 3) error stop 2
    call check("T1T2T3", 3)

    ! The final subroutine of the type, then that of the parent type.
    allocate(sf)
    sf%v = 1
    deallocate(sf)
    call check("F1T1", 4)
    allocate(af(3))
    af%v = [1, 2, 3]
    deallocate(af)
    call check("F1F2F3T1T2T3", 5)

    ! Two levels of extension above the one with a final subroutine.
    allocate(sg)
    sg%v = 4
    deallocate(sg)
    call check("F4T4", 6)
    allocate(ag(2, 2))
    ag%v = reshape([1, 2, 3, 4], [2, 2])
    deallocate(ag)
    call check("F1F2F3F4T1T2T3T4", 7)

    ! A polymorphic entity is finalized as its dynamic type, once.
    allocate(e :: ct)
    ct%v = 5
    deallocate(ct)
    call check("T5", 8)
    allocate(g :: ct)
    ct%v = 6
    deallocate(ct)
    call check("F6T6", 9)
    if (nfin /= 14) error stop 10
end program
