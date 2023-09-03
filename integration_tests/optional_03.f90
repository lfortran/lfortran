module optional_03_module
    implicit none
    type :: string
        character(:), allocatable :: s
    end type
end module optional_03_module

program optional_03
    use optional_03_module
    implicit none
    type(string) :: x(1)
    allocate(character(5) :: x(1)%s)
    x(1)%s = "12345"

    call f(x)
    call f()

contains
    subroutine f(x)
    type(string), optional, intent(in) :: x(:)
    if ( present(x) ) then
        if ( x(1)%s /= "12345" ) error stop
    end if
    end subroutine
end program
