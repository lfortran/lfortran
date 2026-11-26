program assoc_subprogram_return_01
    integer :: x
    x = 1
    call s(x)
contains
    subroutine s(a)
        integer, intent(inout) :: a
        associate(b => a)
            b = b + 1
            return
        end associate
    end subroutine
end program
