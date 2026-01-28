program print_09
    implicit none
    integer, parameter :: dp = kind(0.d0)
    print *, 0.14, 123.456, 0.001, 1.5e-5
    print *, 0.14_dp, 456.789_dp, 0.0001_dp
    write(*,*) 0.14, 123.456, 0.001, 1.5e-5
    write(*,*) 0.14_dp, 456.789_dp, 0.0001_dp
end program print_09
