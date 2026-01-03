program main
    use iso_fortran_env, only: wp => real64
    implicit none

    call compare_solutions()

    contains

    subroutine compare_solutions()
    implicit none
    double precision :: reldiff(2)
    double precision :: absdiff
    reldiff = 0.0_wp
    absdiff = 0.5_wp
    
    where (solution() /= 0.0_wp) reldiff = absdiff / abs(solution())

    if (abs(reldiff(1) - 5.0_wp) > 1e-7) error stop
    if (abs(reldiff(2) - 5.0_wp) > 1e-7) error stop

    print *, reldiff

    end subroutine compare_solutions

    pure function solution() result(x)
        implicit none
        double precision :: x(2)
        x = [0.10_wp,0.10_wp]
    end function solution
end program main

