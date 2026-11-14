! A target dummy can already be the target of a pointer the caller associated
! with the actual argument. `reader` only evaluates that pointer, so the
! launch that writes the dummy has to be finished before `call reader`, in
! the external procedure and in the module procedure. `tick` only increments
! a module integer.
!
! `reader_component` evaluates `g%p` after `g%p => coef`. The pointer
! component reaches other storage, so the launch that writes `coef` has to
! be finished before that call.
!
! `read_a` and the direct read of `a` name the actual argument of an
! assumed-shape target dummy. Both have to see the value the launch wrote.
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
        if (ticks == 1) then
            if (any(abs(p - 2) > 0)) error stop 1
        else if (ticks == 2) then
            if (any(abs(p - 4) > 0)) error stop 2
        else
            error stop 3
        end if
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
        if (any(abs(g%p - 2) > 0)) error stop 4
    end subroutine
    subroutine read_a()
        if (any(abs(a - 8) > 0)) error stop 5
    end subroutine
end module

subroutine helper(x)
    use reader_fields, only: reader, tick
    implicit none
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
    implicit none
    integer :: i
    do concurrent (i = 1:size(coef))
        coef(i) = 2*coef(i)
    end do
    call reader_component()
end subroutine

subroutine helper_actual(x)
    use reader_fields, only: a, tick, read_a
    implicit none
    real, target :: x(:)
    integer :: i
    do concurrent (i = 1:size(x))
        x(i) = 2*x(i)
    end do
    call tick()
    call read_a()
    if (any(abs(a - 8) > 0)) error stop 6
end subroutine

program gpu_target_dummy_reader
    use reader_fields
    implicit none
    interface
        subroutine helper(x)
            real, target :: x(:)
        end subroutine
        subroutine helper_coef()
        end subroutine
        subroutine helper_actual(x)
            real, target :: x(:)
        end subroutine
    end interface
    a = 1
    coef = 1
    p => a
    g%p => coef
    call helper(a)
    call helper_mod(a)
    call helper_coef()
    call helper_actual(a)
    if (ticks /= 3) error stop 7
    if (any(abs(a - 8) > 0)) error stop 8
    if (any(abs(coef - 2) > 0)) error stop 9
end program
