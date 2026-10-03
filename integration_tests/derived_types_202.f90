module derived_types_202_mod
    implicit none

    ! Number of times each final subroutine was called, and the order in
    ! which they ran (seq is built digit by digit: 1 for tf, 2 for v).
    integer :: nfin_tf = 0
    integer :: nfin_v = 0
    integer :: seq = 0

    type :: tf
        integer :: n = 0
    contains
        final :: finalize_tf
    end type

    type :: u
        type(tf) :: c
    end type

    type :: w
        type(u) :: b
    end type

    type :: v
        type(tf) :: c
    contains
        final :: finalize_v
    end type

contains

    subroutine finalize_tf(self)
        type(tf), intent(inout) :: self
        nfin_tf = nfin_tf + 1
        seq = seq*10 + 1
    end subroutine

    subroutine finalize_v(self)
        type(v), intent(inout) :: self
        nfin_v = nfin_v + 1
        seq = seq*10 + 2
    end subroutine

    subroutine reset_direct(x)
        type(tf), intent(out) :: x
    end subroutine

    subroutine reset_component(x)
        type(u), intent(out) :: x
    end subroutine

    subroutine reset_nested(x)
        type(w), intent(out) :: x
    end subroutine

    subroutine reset_array(x)
        type(u), intent(out) :: x(3)
    end subroutine

    subroutine reset_allocatable(x)
        type(u), allocatable, intent(out) :: x
    end subroutine

    subroutine reset_pointer(x)
        type(u), pointer, intent(out) :: x
    end subroutine

    subroutine reset_both(x)
        type(v), intent(out) :: x
    end subroutine

end module

program derived_types_202
    use derived_types_202_mod
    implicit none
    call run()

contains

    subroutine run()
        type(tf) :: a
        type(u) :: b
        type(w) :: c
        type(u) :: d(3)
        type(u), allocatable :: e
        type(u), pointer :: f
        type(u), target :: ftarget
        type(v) :: g

        ! A dummy declared with the finalizable type itself.
        nfin_tf = 0
        call reset_direct(a)
        if (nfin_tf /= 1) error stop 1

        ! A component of the finalizable type.
        nfin_tf = 0
        call reset_component(b)
        if (nfin_tf /= 1) error stop 2

        ! A component of a component.
        nfin_tf = 0
        call reset_nested(c)
        if (nfin_tf /= 1) error stop 3

        ! An array dummy: every element's component is finalized.
        nfin_tf = 0
        call reset_array(d)
        if (nfin_tf /= 3) error stop 4

        ! An unallocated allocatable dummy is not finalized.
        nfin_tf = 0
        call reset_allocatable(e)
        if (nfin_tf /= 0) error stop 5

        ! An allocated one is, because it is deallocated on entry.
        allocate(e)
        nfin_tf = 0
        call reset_allocatable(e)
        if (nfin_tf /= 1) error stop 6
        if (allocated(e)) error stop 7

        ! A pointer dummy is not finalized, and neither is its target. The
        ! target is a local variable rather than an allocation, because an
        ! intent(out) pointer dummy leaves the association status undefined,
        ! so an allocated target could not be freed afterwards and would be
        ! reported by --detect-leaks.
        f => ftarget
        nfin_tf = 0
        call reset_pointer(f)
        if (nfin_tf /= 0) error stop 8

        ! Both the type's own final subroutine and the one of its component
        ! run, the type's own one first.
        nfin_tf = 0
        nfin_v = 0
        seq = 0
        call reset_both(g)
        if (nfin_v /= 1) error stop 9
        if (nfin_tf /= 1) error stop 10
        if (seq /= 21) error stop 11
    end subroutine

end program
