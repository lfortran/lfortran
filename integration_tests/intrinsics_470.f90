program intrinsics_470
    implicit none
    character(kind=4, len=6) :: string
    character(kind=4, len=2) :: substring

    string = 4_"banana"
    substring = 4_"an"

    if (index(string, substring) /= 2) error stop
    if (index(string, substring, back=.true.) /= 4) error stop
    if (index(string, 4_"zz") /= 0) error stop
end program
