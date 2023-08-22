module nested_vars1
    implicit none

    contains

    subroutine h(x)
    integer, intent(inout) :: x
    x = x + 1
    call g()

    contains

        subroutine g()
        x = x + 1
        end subroutine

    endsubroutine

end module


program nested_vars1_main
    use nested_vars1, only: h
    implicit none
    integer :: x
    x = 5
    call h(x)
    print *, x
    if (x /= 7) error stop
end program
