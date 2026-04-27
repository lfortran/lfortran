! Test: BLOCK DATA with character array and scalar in COMMON block
! Verifies that character arrays in COMMON blocks with DATA initialization
! compile without crashing (regression test for LLVM assertion failure).

subroutine test()
    implicit none
    character(1) :: c(1), e
    common /blk1/ c, e

    ! Verify the scalar character is accessible
    if (len(e) /= 1) error stop
end subroutine

program block_data_string_02
    implicit none

    call test()

end program

block data bd1
    implicit none
    character(1) :: c(1), e
    common /blk1/ c, e
    data c, e /'X', 'Y'/
end block data
