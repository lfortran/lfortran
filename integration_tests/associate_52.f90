program associate_51
    implicit none
    character(len=50) :: s
    s = repeat('*', len(s))
    call sub(s)
    if (s /= "**123*********************************************") error stop
    print *, "test passed"
contains
    subroutine sub(str)
        character(len=*), intent(inout) :: str
        associate(substr => str(3:5))
            substr = '123'
        end associate
    end subroutine sub
end program associate_51