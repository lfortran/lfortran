program transfer_32
    character(len=20) :: out

    out = make_string("abc")
    if (out(1:3) /= "abc") error stop

contains

    character(len=20) function make_string(text)
        character(len=*) :: text

        make_string = transfer(merge(transfer(text, "x", len(text)), &
            ["a", "b", "c"], .true.), repeat("x", len(text)))
    end function make_string

end program transfer_32
