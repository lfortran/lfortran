program gpu_metal_645
    ! A DO CONCURRENT kernel writes an array of derived type through a
    ! pointer whose target is not known where the loop is offloaded: one
    ! associated with several targets, or a pointer dummy argument. The
    ! host waits for the kernel before it reads what the pointer may point
    ! to (lfortran/lfortran#16644).
    implicit none
    type :: record_t
        integer :: v
        real :: x
    end type
    type(record_t), target :: a(4), b(4), c(4)
    type(record_t), pointer :: p(:), q(:)
    integer :: i, k

    ! Associated with two targets here.
    a%v = 0
    a%x = 0
    b%v = 0
    p => b
    p => a
    do concurrent (i = 1:4)
        p(i)%v = 10 * i
        p(i)%x = real(i)
    end do
    if (a(1)%v /= 10) error stop 1
    if (any(a%v /= [10, 20, 30, 40])) error stop 2
    if (any(b%v /= 0)) error stop 3
    if (abs(a(4)%x - 4.0) > 1.0e-6) error stop 4

    ! Associated again by a contained procedure.
    c%v = 0
    call redirect()
    do concurrent (i = 1:4)
        p(i)%v = i + 1
    end do
    if (c(1)%v /= 2) error stop 5
    if (any(c%v /= [2, 3, 4, 5])) error stop 6

    ! Associated with a target local of a block, and then with a variable
    ! outside it; the host reads the block local through its name.
    do k = 1, 2
        block
            type(record_t), target :: e(4)
            e%v = 0
            p => e
            do concurrent (i = 1:4)
                p(i)%v = 3 * i + k
            end do
            if (e(3)%v /= 9 + k) error stop 9
            p => c
            do concurrent (i = 1:4)
                p(i)%v = p(i)%v + 1
            end do
        end block
    end do
    if (any(c%v /= [4, 5, 6, 7])) error stop 10

    ! A pointer dummy argument associated in the procedure.
    call fill(q)
    if (any(q%v /= [7, 14, 21, 28])) error stop 8
    print *, a(4)%v, c(4)%v, q(4)%v
contains
    subroutine redirect()
        p => c
    end subroutine

    subroutine fill(r)
        type(record_t), pointer, intent(out) :: r(:)
        type(record_t), target, save :: d(4)
        integer :: j
        d%v = 0
        r => d
        do concurrent (j = 1:4)
            r(j)%v = 7 * j
        end do
        if (d(2)%v /= 14) error stop 7
    end subroutine
end program
