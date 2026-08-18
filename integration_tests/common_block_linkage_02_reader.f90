subroutine read_name(out)
    implicit none
    character(32), intent(out) :: out
    character(32) :: name
    common /sn/ name
    out = name
end subroutine read_name
