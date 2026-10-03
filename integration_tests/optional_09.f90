module optional_09_mod
    implicit none
contains
    subroutine sub(x)
        class(*), intent(in), optional :: x(:)
        if (present(x)) then
            select type(x)
            type is (integer)
                if (x(1) /= 10) error stop
                if (x(2) /= 20) error stop
            class default
                error stop
            end select
        end if
    end subroutine
end module

program optional_09
    use optional_09_mod
    implicit none
    integer :: a(2)
    a = [10, 20]
    call sub()
    call sub(a)
    print *, "ok"
end program
