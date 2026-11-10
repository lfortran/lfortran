! A locally typed name referenced as a function is an external function
! defined later in the same file (#4376)
program implicit_interface_121
    implicit none
    double precision :: b(2, 3)
    call r8_mxm(2, 3, b)
    print *, b(1, 1), b(2, 3)
    if (abs(b(1, 1) - 7.0d0) > 1.0d-12) error stop
    if (abs(b(2, 3) - 1.0d0) > 1.0d-12) error stop
end program implicit_interface_121

subroutine r8_mxm( l, m, b )
implicit none
integer :: l, m
double precision :: b(l,m)
integer :: seed
double precision :: r8_uniform_01
b = 1.0d0
seed = 7
b(1,1) = r8_uniform_01( seed )
end

function r8_uniform_01( seed )
implicit none
double precision :: r8_uniform_01
integer :: seed
r8_uniform_01 = real(seed, kind=8)
return
end
