! Fortran 2023 16.9.202: the integer arguments of system_clock shall have the
! same kind and at least the decimal exponent range of default integer.
program system_clock_f23_01
    implicit none
    integer(1) :: c1
    integer(2) :: c2, r2, m2
    integer(4) :: c4, r4, m4
    integer(8) :: c8, r8, m8
    real(4) :: rr4
    real(8) :: rr8

    ! valid in Fortran 2023
    call system_clock(c4, r4, m4)
    call system_clock(c8, r8, m8)
    call system_clock(count=c4, count_rate=rr4, count_max=m4)
    call system_clock(count=c8, count_rate=rr8, count_max=m8)
    call system_clock(count=c4, count_rate=rr8)
    call system_clock(count_rate=rr4)

    ! integer arguments of different kinds
    call system_clock(count=c8, count_max=m4)
    call system_clock(c4, r8)
    call system_clock(count_rate=r4, count_max=m8)
    call system_clock(c8, rr4, m4)

    ! integer arguments with a smaller range than default integer
    call system_clock(count=c2)
    call system_clock(count=c1)
    call system_clock(count_rate=r2)
    call system_clock(count_max=m2)
    call system_clock(c2, r2, m2)
end program
