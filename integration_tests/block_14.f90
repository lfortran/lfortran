! Test: nested blocks with VLA inside a loop must not leak stack space.
! Each iteration allocates block-scoped variables via alloca; stacksave/
! stackrestore must reclaim the space so the stack does not overflow.
program block_14
    implicit none
    integer :: n, i
    n = 3
    do i = 1, 20000
        block
            real :: a(n, n)
            a = 0.0
            block
                real :: b(3, 3), c(3)
                b = 0.0
                c = 0.0
            end block
            a = a + 1.0
            if (a(1, 1) /= 1.0) error stop
        end block
    end do
    print *, "ok"
end program block_14
