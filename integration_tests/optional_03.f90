module test_optional_module_01
    implicit none
    type :: string
        character(:), allocatable :: s
    end type
contains
    subroutine sub_01(x)
        type(string), optional, intent(inout) :: x(:)
        if (present(x)) then
            x(1)%s = "presentx"
        end if
    end subroutine
end module test_optional_module_01

module test_optional_module_02
    use test_optional_module_01, only: sub_01, string
    implicit none
contains
    subroutine sub_02()
        type(string) :: strdt(1)
        call sub_01()
        call sub_01(strdt)
        print *, strdt(1)%s
        if( strdt(1)%s /= "presentx" ) error stop
    end subroutine sub_02

end module test_optional_module_02

program optional_03
use test_optional_module_02, only: sub_02
implicit none

call sub_02()

end program
