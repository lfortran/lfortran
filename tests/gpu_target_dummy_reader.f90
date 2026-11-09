! A target dummy can already be the target of a pointer the caller associated
! with the actual argument. `reader` only evaluates that pointer. The launch
! that writes the dummy is synced before `call reader`, inside the procedure
! that launched it: the external twin and the module procedure. `tick` only
! increments a module integer, so it is not ordered with the launch.
!
! `reader_component` evaluates `g%p` after `g%p => coef`. The pointer
! component reaches other storage, so the launch that writes `coef` is
! synced before that call.
!
! `read_a` and the direct read of `a(1)` name the actual argument of an
! assumed-shape target dummy. Both are ordered with the launch that writes
! the dummy. `tick` is not.
module reader_fields
    implicit none
    real, pointer :: p(:)
    integer :: ticks = 0
    type :: holder
        real, pointer :: p(:)
    end type holder
    type(holder) :: g
    real, target :: coef(4)
    real, target :: a(4)
contains
    subroutine reader()
        if (p(1) < -1.0e30) error stop
    end subroutine
    subroutine tick()
        ticks = ticks + 1
    end subroutine
    subroutine helper_mod(x)
        real, target :: x(:)
        integer :: i
        do concurrent (i = 1:size(x))
            x(i) = 2*x(i)
        end do
        call tick()
        call reader()
    end subroutine
    subroutine reader_component()
        if (g%p(1) < -1.0e30) error stop
    end subroutine
    subroutine read_a()
        if (a(1) < -1.0e30) error stop
    end subroutine
end module

subroutine helper(x)
    use reader_fields, only: reader, tick
    real, target :: x(:)
    integer :: i
    do concurrent (i = 1:size(x))
        x(i) = 2*x(i)
    end do
    call tick()
    call reader()
end subroutine

subroutine helper_coef()
    use reader_fields, only: coef, reader_component
    integer :: i
    do concurrent (i = 1:size(coef))
        coef(i) = 2*coef(i)
    end do
    call reader_component()
end subroutine

subroutine helper_actual(x)
    use reader_fields, only: a, tick, read_a
    real, target :: x(:)
    integer :: i
    do concurrent (i = 1:size(x))
        x(i) = 2*x(i)
    end do
    call tick()
    call read_a()
    if (a(1) < -1.0e30) error stop
end subroutine

program reader_sync
    use reader_fields
    implicit none
    a = 1
    coef = 1
    p => a
    g%p => coef
    call helper(a)
    call helper_mod(a)
    call helper_coef()
    call helper_actual(a)
end program
