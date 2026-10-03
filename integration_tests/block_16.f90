module block_16_mod
    implicit none
contains
    subroutine s1(x)
        real, intent(out) :: x
        block
            real :: a(2) = [1.0, 2.0]
            x = a(1) + a(2)
        end block
    end subroutine
    subroutine s2(x)
        real, intent(out) :: x
        block
            real :: a(3) = [3.0, 4.0, 5.0]
            x = a(1) + a(2) + a(3)
        end block
    end subroutine
end module

program block_16
    use block_16_mod
    implicit none
    real :: r1, r2
    call s1(r1)
    call s2(r2)
    print *, r1, r2
    if (abs(r1 - 3.0) > 1e-6) error stop
    if (abs(r2 - 12.0) > 1e-6) error stop
end program
