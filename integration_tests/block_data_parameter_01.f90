! Test for PARAMETER statement handling in block data units
! Requires --implicit-typing flag
program block_data_param_test
    implicit none
    real :: a(2)
    common /blk1/ a
    
    if (size(a) /= 2) error stop "Array size incorrect"
    print *, "Test Passed"
end program

block data bdata_1
    parameter (KPI = 2)  ! Uses implicit typing
    real a(KPI)
    common /blk1/ a
    data a /1.0, 2.0/
end block data
