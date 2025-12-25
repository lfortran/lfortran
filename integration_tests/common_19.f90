! Test: CHARACTER variables in COMMON blocks
! Verifies storage association with character types
program common_19
    implicit none
    character(len=8) :: str1
    character(len=4) :: str2
    integer :: icheck
    common /charblk/ str1, str2, icheck

    str1 = "ABCDEFGH"
    str2 = "1234"
    icheck = 999

    call sub_same_layout()
    call sub_diff_names()
    print *, "PASS: common_19"
end program

subroutine sub_same_layout()
    implicit none
    ! Same layout as main program
    character(len=8) :: s1
    character(len=4) :: s2
    integer :: ival
    common /charblk/ s1, s2, ival

    ! Verify values match what was set in main
    if (s1 /= "ABCDEFGH") error stop "s1 should be ABCDEFGH"
    if (s2 /= "1234") error stop "s2 should be 1234"
    if (ival /= 999) error stop "ival should be 999"

    ! Modify values
    s1 = "XXXXXXXX"
    s2 = "YYYY"
    ival = 123
end subroutine

subroutine sub_diff_names()
    implicit none
    ! Different variable names, same layout
    character(len=8) :: name1
    character(len=4) :: name2
    integer :: num
    common /charblk/ name1, name2, num

    ! Verify modified values from sub_same_layout
    if (name1 /= "XXXXXXXX") error stop "name1 should be XXXXXXXX"
    if (name2 /= "YYYY") error stop "name2 should be YYYY"
    if (num /= 123) error stop "num should be 123"
end subroutine
