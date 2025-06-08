module procedure_09_module
contains
    subroutine cb(x, a, b, terminate)
        implicit none
        real, intent(in) :: x(:)
        integer, intent(in), optional :: a
        integer, intent(in), optional :: b
        logical, intent(out), optional :: terminate
        if(present(a) .or. present(b) .and. .not. present(terminate)) error stop
    end subroutine cb
end module procedure_09_module
