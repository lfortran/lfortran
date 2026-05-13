subroutine r8_mxm(l, m)
    implicit none
    integer :: l, m
    double precision :: b(l, m)
    integer :: seed
    double precision :: r8_uniform_01

    seed = 3
    b(1, 1) = r8_uniform_01(seed)
    if (abs(b(1, 1) - 3.0d0) > 1.0d-12) error stop
end subroutine

double precision function r8_uniform_01(seed)
    implicit none
    integer :: seed
    r8_uniform_01 = real(seed, kind=8)
end function

program external_17
    implicit none
    call r8_mxm(1, 1)
end program