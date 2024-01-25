program intrinsic_126
    implicit none

    integer :: arg = 0
    real :: num1, num2

    call system_clock(arg)

    call srand(arg)

    call random_number(num1)
    call random_number(num2)

    print *, arg, num1, num2

    ! Ensure num1 and num2 are different
    if (abs(num1 - num2) <= 1e-8) error stop

end program
