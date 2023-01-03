module interface_10_m
    interface chars
        module procedure identity
    end interface

contains

    subroutine identity(x, y)
        character, intent(in) :: x
        character(kind=1), intent(out) :: y
        y = x
    end subroutine
end module

program interface_10
    use interface_10_m
    character :: x, y
    x = 'A'
    call chars(x, y)
    print *, x
end program
