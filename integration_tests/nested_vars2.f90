module nested_vars2
    implicit none

    contains

    subroutine h(x)
    integer, intent(inout) :: x
    x = x + 1
    return
    call g()

    contains

        subroutine g()
        x = x + 1
        end subroutine

    end subroutine

end module


program nested_vars2_main
    use nested_vars2, only: h
    implicit none
    integer :: x
    x = 5
    call h(x)
    if (x /= 6) error stop
end program
