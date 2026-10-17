! A dummy argument may appear in only one type declaration statement,
! also when implicit typing is enabled.
program implicit_typing5
    interface
        subroutine f(x)
            real :: x
            type(*), dimension(..) :: x
        end subroutine
    end interface
    integer :: a(10)
    call f(a)
end program
