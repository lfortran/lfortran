subroutine set_common_name()
    implicit none
    character(4) :: name
    common /nm_53/ name
    name = "ABCD"
end subroutine set_common_name
