module gpu_metal_648_mod
    implicit none
    type :: record_t
        integer :: v
        real :: x
    end type
    type(record_t), pointer :: gp(:)
contains
    subroutine point_module(x)
        type(record_t), target, intent(inout) :: x(:)
        gp => x
    end subroutine

    subroutine point_dummy(p, x)
        type(record_t), pointer, intent(inout) :: p(:)
        type(record_t), target, intent(inout) :: x(:)
        p => x
    end subroutine

    subroutine by_dummy(p)
        ! A pointer dummy, associated with a block-local target by a call.
        type(record_t), pointer, intent(inout) :: p(:)
        integer :: i, k
        do k = 1, 3
            block
                type(record_t), target :: e(256)
                e%v = 0
                e%x = 0
                call point_dummy(p, e)
                do concurrent (i = 1:256)
                    p(i)%v = 1000 + i
                    p(i)%x = 1.0
                end do
            end block
            block
                type(record_t) :: f(256)
                f%v = k
                f%x = 2.0
                print *, k, f(1)%v, f(256)%v
                if (any(f%v /= k)) error stop 7
                if (any(f%x /= 2.0)) error stop 8
            end block
        end do
        nullify(p)
    end subroutine
end module

program gpu_metal_648
    ! A DO CONCURRENT kernel writes a target local of a block through a
    ! pointer that is never associated in the procedure that offloads the
    ! loop: a module pointer, a host-associated pointer and a pointer dummy,
    ! each associated with the local by a call. The host waits for the
    ! kernel before the block ends: a later block reuses its storage
    ! (lfortran/lfortran#16644).
    use gpu_metal_648_mod
    implicit none
    type(record_t), target :: c(256)
    type(record_t), pointer :: hp(:), q(:)
    integer :: i, k

    c%v = 0
    c%x = 0
    call point_module(c)
    do k = 1, 3
        block
            type(record_t), target :: e(256)
            e%v = 0
            e%x = 0
            call point_module(e)
            do concurrent (i = 1:256)
                gp(i)%v = 1000 + i
                gp(i)%x = 1.0
            end do
        end block
        block
            type(record_t) :: f(256)
            f%v = k
            f%x = 2.0
            ! Waits for every launch: anything still pending is copied
            ! back here.
            print *, k, f(1)%v, f(256)%v
            if (any(f%v /= k)) error stop 1
            if (any(f%x /= 2.0)) error stop 2
        end block
    end do
    nullify(gp)

    hp => c
    call by_host()
    nullify(hp)

    q => c
    call by_dummy(q)

    if (any(c%v /= 0)) error stop 9
    print *, "ok"
contains
    subroutine point_host(x)
        type(record_t), target, intent(inout) :: x(:)
        hp => x
    end subroutine

    subroutine by_host()
        ! A host-associated pointer, associated with a block-local target
        ! by a call.
        integer :: i, k
        do k = 1, 3
            block
                type(record_t), target :: e(256)
                e%v = 0
                e%x = 0
                call point_host(e)
                do concurrent (i = 1:256)
                    hp(i)%v = 1000 + i
                    hp(i)%x = 1.0
                end do
            end block
            block
                type(record_t) :: f(256)
                f%v = k
                f%x = 2.0
                print *, k, f(1)%v, f(256)%v
                if (any(f%v /= k)) error stop 4
                if (any(f%x /= 2.0)) error stop 5
            end block
        end do
    end subroutine
end program
