program format_49
    ! Test E format exponent adjustment when mantissa rounds to 0.999...
    ! Bug: exponent was off by one for extreme values like 1.0d-200
    ! when using high precision formats like E25.17
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: val
    character(30) :: output
    integer :: exp_val, i, exp_start

    ! 1.0d-200 with E25.17 format:
    ! Mantissa rounds to 0.99999999999999998
    ! So exponent must be -200 (not -199)
    val = 1.0d-200
    write(output, '(E25.17)') val

    ! Find the exponent sign position (last - or + in the string)
    exp_start = 0
    do i = len_trim(output), 1, -1
        if (output(i:i) == '-' .or. output(i:i) == '+') then
            exp_start = i
            exit
        end if
    end do

    ! Parse exponent from the sign position
    read(output(exp_start:), '(I4)') exp_val

    if (exp_val /= -200) then
        print *, "FAIL: 1.0d-200 exponent should be -200, got ", exp_val
        print *, "Output was: ", trim(output)
        error stop
    end if

    print *, "PASS: E format exponent correctly adjusted for 1.0d-200"
end program
