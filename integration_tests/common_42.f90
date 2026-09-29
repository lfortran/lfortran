! A COMMON block CHARACTER member is stored inline as a flat byte blob, but a
! callee taking a string descriptor needs one materialized over those bytes.
! Passing the member straight through produced a call whose argument type did
! not match the callee's signature (reduced from LAPACK's srnamc / xerbla).
! The same holds for a CHARACTER array member: the whole array is one
! descriptor over the blob's bytes, with the element length as its length.
program common_42
    implicit none
    character(32) :: srnamt
    character(8) :: tag
    character(2) :: ca(2)
    integer :: neighbour
    common /srnamc/ srnamt
    common /tagc/ tag
    ! a CHARACTER array member, stored inline next to a non-character one
    common /arrc/ ca, neighbour

    srnamt = "ABCDE"
    tag = "xy"
    ca(1) = "ab"
    ca(2) = "cd"
    neighbour = 7

    ! intrinsic taking a string descriptor
    if (len_trim(srnamt) /= 5) error stop "len_trim on common char member"
    if (trim(srnamt) /= "ABCDE") error stop "trim on common char member"
    if (index(srnamt, "CD") /= 3) error stop "index on common char member"

    ! user-defined function taking a string
    if (mylen(srnamt) /= 5) error stop "user function on common char member"
    if (mylen(tag) /= 2) error stop "user function on second common block"

    ! a whole CHARACTER array member also needs a descriptor over its bytes
    if (ca(1) /= "ab") error stop "element of common char array member"
    if (ca(2) /= "cd") error stop "element of common char array member"
    if (len(ca) /= 2) error stop "len of common char array member"
    if (size(ca) /= 2) error stop "size of common char array member"
    if (any(ca /= ["ab", "cd"])) error stop "whole common char array member"
    call take_ca(ca)
    if (neighbour /= 7) error stop "neighbour of common char array member"

    print *, len_trim(srnamt), trim(srnamt), mylen(tag)
    print *, ca, neighbour

contains

    subroutine take_ca(x)
        character(len=2), intent(in) :: x(2)
        if (x(1) /= "ab") error stop "first element passed from common"
        if (x(2) /= "cd") error stop "second element passed from common"
        if (len(x) /= 2) error stop "len of array passed from common"
        if (size(x) /= 2) error stop "size of array passed from common"
    end subroutine take_ca

    integer function mylen(s)
        character(len=*), intent(in) :: s
        mylen = len_trim(s)
    end function mylen

end program common_42
