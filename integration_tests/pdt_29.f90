program pdt_29
    ! The output of a whole object lists its components but not its LEN
    ! type parameters.
    implicit none

    type :: tag_t(n)
        integer, len :: n
        integer :: id
        character(len=n) :: name
    end type

    type(tag_t(4)) :: t
    character(len=64) :: line

    t%id = 42
    t%name = "pdt"
    write(line, "(i0,1x,a,a)") t, "|"
    if (trim(line) /= "42 pdt |") error stop
    write(line, *) t
    if (trim(adjustl(line)) /= "42 pdt") error stop
    print *, t
end program
