module optional_03_module_01
    implicit none
    type :: string
        character(:), allocatable :: s
    end type
contains
    subroutine sub_01(x)
        type(string), optional, intent(in) :: x(:)
        if ( present(x) ) then
            if ( x(1)%s /= "12345" ) error stop
        end if
    end subroutine
end module optional_03_module_01

module optional_03_module_02
    use optional_03_module_01, only: sub_01
    implicit none
contains
    subroutine sub_02()
        call sub_01()
    end subroutine sub_02

end module optional_03_module_02

program optional_03
    use optional_03_module_01
    use optional_03_module_02, only: sub_02
    implicit none
    type(string) :: x(1)
    allocate(character(5) :: x(1)%s)
    x(1)%s = "12345"

    call sub_01(x)
    call sub_02()
end program
