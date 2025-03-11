module string_51_mod
    contains

    subroutine ff(x, y)
        implicit none
        character(len=*), intent(in) :: x
        character(len=len(x)), intent(out) :: y
        y = trim(x)
    end subroutine 
end module 


program string_51
    use string_51_mod
    character(:), allocatable :: s
    allocate(character(6) :: s)
    call ff("hello ",s)
    print *,"|", s, "|"
    if(s /= "hello") error stop
end program 