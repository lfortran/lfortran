! A COMMON block CHARACTER member is stored inline as a flat byte blob, but a
! callee taking a string descriptor needs one materialized over those bytes.
! Passing the member straight through produced a call whose argument type did
! not match the callee's signature (reduced from LAPACK's srnamc / xerbla).
program common_42
    implicit none
    character(32) :: srnamt
    character(8) :: tag
    common /srnamc/ srnamt
    common /tagc/ tag

    srnamt = "ABCDE"
    tag = "xy"

    ! intrinsic taking a string descriptor
    if (len_trim(srnamt) /= 5) error stop "len_trim on common char member"
    if (trim(srnamt) /= "ABCDE") error stop "trim on common char member"
    if (index(srnamt, "CD") /= 3) error stop "index on common char member"

    ! user-defined function taking a string
    if (mylen(srnamt) /= 5) error stop "user function on common char member"
    if (mylen(tag) /= 2) error stop "user function on second common block"

    print *, len_trim(srnamt), trim(srnamt), mylen(tag)

contains

    integer function mylen(s)
        character(len=*), intent(in) :: s
        mylen = len_trim(s)
    end function mylen

end program common_42
