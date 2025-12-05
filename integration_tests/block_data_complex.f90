program bd_test
implicit none
complex :: axvc
common /blk9/ axvc
if (abs(real(axvc) - 234.23) > 0.01) error stop "Real part incorrect"
if (abs(aimag(axvc) - 34.9) > 0.01) error stop "Imaginary part incorrect"
print *, "AXVC from MAIN =", axvc
print*,"Test Passed"
end program

block data
complex :: axvc
common /blk9/ axvc
data axvc /(234.23, 34.9)/
end block data
