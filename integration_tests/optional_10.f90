module optional_10_mod
    implicit none
    type :: t
    contains
        procedure :: s
    end type
    type :: u
        type(t), allocatable :: c
    end type
contains
    subroutine s(self, x)
        class(t), intent(in) :: self
        integer, intent(in), optional :: x
        if (present(x)) then
            if (x /= 42) error stop
        end if
    end subroutine
end module

program optional_10
    use optional_10_mod
    implicit none
    type(u) :: obj
    allocate(obj%c)
    call obj%c%s()
    call obj%c%s(42)
    print *, "ok"
end program
