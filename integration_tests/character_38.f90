program character_38
    ! character array passed to an explicit-shape dummy with runtime size (#4234)
    implicit none
    character(1) :: char_arr1(4)
    char_arr1(:) = "A"

    call doit(char_arr1, 4)

contains

    subroutine doit(a, n)
        integer :: n
        character(1) :: a(n)
        integer :: i
        print *, a
        if (size(a) /= 4) error stop
        if (len(a) /= 1) error stop
        do i = 1, n
            if (a(i) /= "A") error stop
        end do
    end subroutine doit

end program
