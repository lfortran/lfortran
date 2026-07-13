module namelist_34_mod
    implicit none
contains
    subroutine set_value(arr)
        real, intent(in) :: arr(:)
        integer :: value
        namelist /settings/ value

        value = size(arr)
        if (value /= 3) error stop
    end subroutine
end module

program namelist_34
    use namelist_34_mod, only: set_value
    implicit none

    call set_value([1.0, 2.0, 3.0])
end program
