program intrinsics_76
implicit none

real(8) :: gamma = 1
real(8) :: x(3), xsqrt(3)
gamma = 0.3612163121_8
x = [1._8, 2._8, 3._8]

if( any(abs(exp(-gamma*sqrt(x)) - [0.69682824978102864_8, 0.59999316978514361_8, 0.53491629349743086_8]) > 1e-6) ) error stop
print *, exp(-gamma*sqrt(x))

xsqrt = -gamma*sqrt(x)
if( any(abs(exp(xsqrt) - [0.69682824978102864_8, 0.59999316978514361_8, 0.53491629349743086_8]) > 1e-6) ) error stop
print *, exp(xsqrt)

end program
