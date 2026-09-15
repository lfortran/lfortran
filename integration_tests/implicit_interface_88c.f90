! Module procedures that reference procedures with implicit interfaces
! declared in another module, used through one and through two levels of
! modules.
module implicit_interface_88_b
    use implicit_interface_88_a
    implicit none
contains
    subroutine ii88_drive(x)
        real, intent(inout) :: x
        call ii88_sub(x)
        x = x + ii88_fun(x)
    end subroutine
end module

module implicit_interface_88_c
    use implicit_interface_88_b
    implicit none
contains
    subroutine ii88_drive2(x)
        real, intent(inout) :: x
        call ii88_drive(x)
        call ii88_sub(x)
    end subroutine
end module
