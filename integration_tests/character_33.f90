program character_33
    implicit none
    character(len=10) :: text
    character :: chars(2, 5)

    call accepts_rank2("a")

    text = "1234567890"
    call copy_to_rank2(text, chars)
    if (chars(1, 1) /= "1") error stop
    if (chars(2, 1) /= "2") error stop
    if (chars(1, 5) /= "9") error stop
    if (chars(2, 5) /= "0") error stop
    print *, "All tests passed."

contains

    subroutine accepts_rank2(x)
        character, dimension(2, *) :: x
    end subroutine

    subroutine copy_to_rank2(source, dest)
        character, dimension(2, *) :: source
        character, dimension(2, 5) :: dest

        dest = source(:, 1:5)
    end subroutine

end program
