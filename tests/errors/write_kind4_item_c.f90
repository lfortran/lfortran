program write_kind4_item_c
    ! The C backend emits every character value as a byte string, so it
    ! cannot write a character item of kind 4.
    implicit none
    write(*, '(a)') 4_"xyz"
end program
