! C1588 forbids only polymorphic intent(out) dummies of pure procedures and
! intent(out) dummies with a polymorphic allocatable ultimate component.
! These nearby cases are valid and must keep compiling.
module pure_intent_03_mod
    implicit none
    type :: t
        integer :: i = 0
    end type
    type :: wp
        class(t), pointer :: p => null()
    end type
    type :: wa
        type(t), allocatable :: c
    end type
    type :: wc
        class(t), allocatable :: c
    end type
    type :: wn
        type(wc), allocatable :: inner
    end type
contains
    impure elemental subroutine set_impure(a)
        class(t), intent(out) :: a
        a%i = 1
    end subroutine

    pure subroutine set_inout(a)
        class(t), intent(inout) :: a
        a%i = a%i + 2
    end subroutine

    pure subroutine set_type(a)
        type(t), intent(out) :: a
        a%i = 3
    end subroutine

    pure integer function get_class(a)
        class(t), intent(in) :: a
        get_class = a%i
    end function

    pure subroutine set_ptr_comp(a)
        type(wp), intent(out) :: a
        a%p => null()
    end subroutine

    pure subroutine set_alloc_comp(a)
        type(wa), intent(out) :: a
        allocate(a%c)
        a%c%i = 4
    end subroutine

    pure subroutine set_alloc_nested(a)
        type(wn), intent(out) :: a
        allocate(a%inner)
    end subroutine

    subroutine set_nonpure(a)
        class(t), intent(out) :: a
        a%i = 5
    end subroutine
end module pure_intent_03_mod

program pure_intent_03
    use pure_intent_03_mod
    implicit none
    type(t) :: x, y(2)
    type(t), target :: z
    type(wp) :: p
    type(wa) :: a
    type(wn) :: n

    call set_impure(x)
    if (x%i /= 1) error stop
    call set_impure(y)
    if (any(y%i /= 1)) error stop
    call set_inout(x)
    if (x%i /= 3) error stop
    call set_type(x)
    if (x%i /= 3) error stop
    if (get_class(x) /= 3) error stop
    p%p => z
    call set_ptr_comp(p)
    if (associated(p%p)) error stop
    call set_alloc_comp(a)
    if (.not. allocated(a%c)) error stop
    if (a%c%i /= 4) error stop
    call set_alloc_nested(n)
    if (.not. allocated(n%inner)) error stop
    if (allocated(n%inner%c)) error stop
    call set_nonpure(x)
    if (x%i /= 5) error stop
    print *, "ok"
end program
