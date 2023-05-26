program allocate_06
    implicit none
    character(len=:), allocatable :: array(:)
    call sub(array)
    if (array(1) /= "abc") error stop
    print *, array(1)

contains

    subroutine sub(x_array)
        character(len=:), allocatable, intent(out) :: x_array(:)
        allocate(character(len=8)::x_array(2))
        x_array = ["abc"]
        if (x_array(1) /= "abc") error stop
        print *, x_array(1)
    end subroutine sub

end program allocate_06
