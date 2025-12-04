program bd_test
implicit none
complex :: axvc
common /blk9/ axvc
print *, "AXVC from MAIN =", axvc
end program

block data
complex :: axvc
common /blk9/ axvc
data axvc /(234.23, 34.9)/
end block data
