program gpu_metal_647
    ! A DO CONCURRENT kernel writes a target local of a block through a
    ! pointer whose target is not known where the loop is offloaded, and
    ! the block ends without reading it. The host waits for the kernel
    ! before the block ends: a later block reuses its storage
    ! (lfortran/lfortran#16644).
    implicit none
    type :: record_t
        integer :: v
        real :: x
    end type
    type(record_t), target :: c(256)
    type(record_t), pointer :: p(:)
    integer :: i, k

    c%v = 0
    c%x = 0
    p => c
    do k = 1, 3
        block
            type(record_t), target :: e(256)
            e%v = 0
            e%x = 0
            p => e
            do concurrent (i = 1:256)
                p(i)%v = 1000 + i
                p(i)%x = 1.0
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
    if (any(c%v /= 0)) error stop 3
    print *, "ok"
end program
