program test_write_implied_do
    use iso_fortran_env, only: output_unit
    implicit none

    integer, parameter :: stdout = output_unit
    integer :: ii, iii
    character(len=20) :: lines(3)

    lines(1) = "first"
    lines(2) = "second"
    lines(3) = "third"

    ii = 3

    print *, ( trim(lines(iii)), iii = 1, ii )
    
    ! Verify that all values were printed
    if (ii /= 3) error stop

end program test_write_implied_do
