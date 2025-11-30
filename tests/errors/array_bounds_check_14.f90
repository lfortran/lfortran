module array_bounds_check_14_mod
contains

    subroutine f(i)
        integer, intent(in) :: i(:)
        print *, i
    end subroutine
end module


program array_bounds_check_14
    use array_bounds_check_14_mod

    integer, allocatable :: x(:)

    call f(x)
end program
