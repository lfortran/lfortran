! Functions whose bodies contain statement labels are referenced more than
! once from the same caller, which also uses the same label numbers.
module goto_10_m
    implicit none
contains
    real(8) function heaviside(t)
        real(8), intent(in) :: t
1       if (t < 0.0d0) then
            heaviside = 0.0d0
        else
            heaviside = 1.0d0
        end if
    end function

    integer function twice(i)
        integer, intent(in) :: i
        twice = 2*i
20      continue
    end function

    real(8) function half(x)
        real(8), intent(in) :: x
30      half = 0.5d0*x
    end function

    integer function sum_to(n)
        integer, intent(in) :: n
        integer :: i
        sum_to = 0
        i = 0
10      i = i + 1
        if (i > n) go to 20
        sum_to = sum_to + i
        go to 10
20      continue
    end function

    integer function in_block(i)
        integer, intent(in) :: i
        in_block = i
40      block
            integer :: j
            j = 3
            in_block = in_block + j
        end block
    end function
end module

program goto_10
    use goto_10_m
    implicit none
    integer :: k, total
    if (heaviside(-1.0d0) /= 0.0d0) error stop 1
    if (heaviside(2.0d0) /= 1.0d0) error stop 2
    if (twice(3) /= 6) error stop 3
    if (twice(-4) /= -8) error stop 4
    if (half(3.0d0) /= 1.5d0) error stop 5
    if (half(-1.0d0) /= -0.5d0) error stop 6
    if (sum_to(4) + sum_to(5) /= 25) error stop 7
    if (in_block(1) + in_block(2) /= 9) error stop 8
    k = 0
    total = 0
10  k = k + 1
    if (k > 3) go to 20
    total = total + sum_to(k) + twice(k)
    go to 10
20  continue
    if (total /= 22) error stop 9
    print *, total
end program
