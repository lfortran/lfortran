! `transfer` of an array constant of length-1 strings to a longer
! character variable used as the mold (#3494).
program transfer_38
    implicit none
    character(len=10) :: string

    string = transfer(["a", "b"], string)
    print *, string(1:2)
    if (string(1:2) /= "ab") error stop 1

    string = transfer(["x", "y", "z"], string)
    if (string(1:3) /= "xyz") error stop 2
end program transfer_38
