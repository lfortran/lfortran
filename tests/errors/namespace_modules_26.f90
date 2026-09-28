! Error: interface bodies do not access their host by host association, so
! the module entity "m" is not accessible without an IMPORT statement.
module namespace_modules_26_m
    implicit none
    type :: t
        integer :: x = 1
    end type
end module

program namespace_modules_26
    use, namespace :: m => namespace_modules_26_m
    implicit none
    interface
        subroutine ext(a)
            type(m%t), intent(in) :: a
        end subroutine
    end interface
end program
