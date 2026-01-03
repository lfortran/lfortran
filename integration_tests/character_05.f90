program character_05
character(1) :: b
b = "X"
call f(b)

contains

    subroutine f(s) bind(c)
    character(1) :: s
    if (s /= "X") error stop
    end subroutine

end program
