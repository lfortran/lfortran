! A character array member of a SEQUENCE (or bind(C)) derived type is stored
! inline in the struct, as a flat blob of count*len bytes, rather than behind
! a string descriptor. The blob has to be sized by the element count, and
! indexing it has to step by the element length.
program derived_types_154
    implicit none
    type :: t
        sequence
        character(len=4) :: names(3)
        integer :: n
    end type
    type(t) :: x

    x%names(1) = "abcd"
    x%names(2) = "efgh"
    x%names(3) = "ijkl"
    x%n = 42

    if (x%names(1) /= "abcd") error stop "names(1)"
    if (x%names(2) /= "efgh") error stop "names(2)"
    if (x%names(3) /= "ijkl") error stop "names(3)"
    ! a substring of an inline element must read the same bytes
    if (x%names(2)(2:3) /= "fg") error stop "names(2)(2:3)"
    if (x%names(3)(1:1) /= "i") error stop "names(3)(1:1)"
    ! the member after the blob must not be overlapped by it
    if (x%n /= 42) error stop "n"

    print *, x%names(1), x%names(2), x%names(3), x%n
end program derived_types_154
