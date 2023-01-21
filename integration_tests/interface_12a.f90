module interface_12a_mod
    use interface_12b_mod

    interface get_value
        module procedure :: get_bool
    end interface

contains

    subroutine get_bool(self, val)
        class(t), intent(in) :: self
        logical, intent(out) :: val
        if (self%value /= 0) then
            val = .true.
        else
            val = .false.
        end if
    end subroutine get_bool

end module interface_12a_mod

program interface_12a
    use interface_12a_mod
    implicit none

    type(t) :: get_type
    logical :: a
    real :: b

    get_type%value = 10

    call get_value(get_type, a)
    call get_value(get_type, b)

    if (.not.(a)) error stop
    if (b /= 10.00) error stop
end program interface_12a

