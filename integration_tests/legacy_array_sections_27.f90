! Inlined explicit-shape dummies retain their bounds over an element sequence.
module legacy_array_sections_27_m
    implicit none
    integer :: calls = 0
contains
    integer function counted(n)
        integer, intent(in) :: n
        calls = calls + 1
        counted = n
    end function counted
end module legacy_array_sections_27_m

program legacy_array_sections_27
    use legacy_array_sections_27_m
    implicit none
    real :: a(4, 3), total
    integer :: i, j

    a = reshape([(real(i), i = 1, 12)], [4, 3])
    if (sum3(a(2, 1)) /= 9) error stop 1
    if (sumn(5, a(3, 2)) /= 45) error stop 2
    i = 2
    if (sumn(i + 1, a(1, 3)) /= 30) error stop 3
    if (tail(a(2, 2), 4) /= 24) error stop 4
    total = 0
    do j = 1, 3
        total = total + sumn(4, a(1, j))
    end do
    if (total /= 78) error stop 5
    calls = 0
    if (sumn(counted(2), a(2, 2)) /= 13) error stop 6
    if (calls /= 1) error stop 7
    if (shifted(a(3, 1)) /= 25) error stop 8
    print '(a)', 'legacy_array_sections_27 ok'
contains
    pure real function sum3(x)
        real, intent(in) :: x(3)
        integer :: k
        sum3 = 0
        do k = 1, 3
            sum3 = sum3 + x(k)
        end do
    end function sum3

    pure real function sumn(n, x)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        integer :: k
        sumn = 0
        do k = 1, n
            sumn = sumn + x(k)
        end do
    end function sumn

    pure real function tail(x, n)
        integer, intent(in) :: n
        real, intent(in) :: x(n)
        integer :: k
        tail = 0
        do k = 2, size(x)
            tail = tail + x(k)
        end do
    end function tail

    pure real function shifted(x)
        real, intent(in) :: x(-2:2)
        integer :: k
        shifted = 0
        do k = lbound(x, 1), ubound(x, 1)
            shifted = shifted + x(k)
        end do
    end function shifted
end program legacy_array_sections_27
