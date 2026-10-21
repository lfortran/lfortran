module gpu_metal_520_m
    ! Pure functions that read module arrays directly, not through their
    ! arguments: the fusion of device loops must see these reads.
    implicit none
    real, allocatable :: f(:), q(:), g2(:)
contains
    ! Reads the intermediate array of `consumer_reads_module_array`.
    pure real function neighbours(i)
        integer, intent(in) :: i
        neighbours = f(i - 1) + f(i + 1)
    end function

    ! Reads the field the consumer of `producer_reads_module_array`
    ! updates.
    pure real function difference(i)
        integer, intent(in) :: i
        difference = q(i + 1) - q(i)
    end function

    ! Reads the intermediate array of `other_statement_reads_module_array`.
    pure real function peek(i)
        integer, intent(in) :: i
        peek = g2(i)
    end function

    subroutine consumer_reads_module_array()
        real, allocatable :: p(:), r(:)
        real :: expected(8)
        integer :: n, s, i
        n = 8
        allocate(p(n), f(n), r(n))
        do i = 1, n
            p(i) = real(i*i)
        end do
        f = -1.0
        r = 0.0
        do s = 1, 3
            do concurrent (i = 1:n)
                f(i) = 2.0 * p(i) + real(s)
            end do
            do concurrent (i = 2:n - 1)
                r(i) = r(i) + f(i) + neighbours(i)
            end do
        end do
        expected = [0.0, 102.0, 192.0, 318.0, 480.0, 678.0, 912.0, 0.0]
        print *, r
        if (any(abs(r - expected) > 1e-3)) error stop
        deallocate(f)
    end subroutine

    subroutine producer_reads_module_array()
        real, allocatable :: h(:), qh(:), hh(:)
        integer :: n, s, i
        n = 8
        allocate(q(n), h(n), qh(n), hh(n))
        do i = 1, n
            q(i) = real(i*i)
        end do
        qh = q
        do s = 1, 3
            do concurrent (i = 1:n - 1)
                h(i) = difference(i)
            end do
            do concurrent (i = 2:n - 1)
                q(i) = q(i) + 0.25 * (h(i) - h(i - 1))
            end do
        end do
        ! The same on the host.
        do s = 1, 3
            do i = 1, n - 1
                hh(i) = qh(i + 1) - qh(i)
            end do
            do i = 2, n - 1
                qh(i) = qh(i) + 0.25 * (hh(i) - hh(i - 1))
            end do
        end do
        print *, q
        if (any(abs(q - qh) > 1e-3)) error stop
        if (abs(q(2) - 5.1875) > 1e-3) error stop
        deallocate(q)
    end subroutine

    subroutine other_statement_reads_module_array()
        real, allocatable :: p(:), r(:)
        real :: total
        integer :: n, s, i
        n = 8
        allocate(p(n), g2(n), r(n))
        do i = 1, n
            p(i) = real(i)
        end do
        g2 = 0.0
        r = 0.0
        total = 0.0
        do s = 1, 3
            do concurrent (i = 1:n)
                g2(i) = 2.0 * p(i) + real(s)
            end do
            do concurrent (i = 2:n - 1)
                r(i) = r(i) + g2(i - 1) + g2(i + 1)
            end do
            total = total + peek(3)
        end do
        print *, total
        ! 7 + 8 + 9
        if (abs(total - 24.0) > 1e-3) error stop
        if (abs(r(2) - 36.0) > 1e-3) error stop
        deallocate(g2)
    end subroutine
end module

program gpu_metal_520
    use gpu_metal_520_m
    implicit none
    call consumer_reads_module_array()
    call producer_reads_module_array()
    call other_statement_reads_module_array()
end program
