! Error: a local generic interface does not extend a generic accessed
! through a namespace, so g%swap has no specific for character arguments.
module namespace_modules_30_gen
    implicit none
    interface swap
        module procedure swap_int
    end interface
contains
    subroutine swap_int(a, b)
        integer, intent(inout) :: a, b
        integer :: t
        t = a; a = b; b = t
    end subroutine
end module

module namespace_modules_30_user
    use, namespace :: g => namespace_modules_30_gen
    implicit none
    interface swap
        module procedure swap_char
    end interface
contains
    subroutine swap_char(a, b)
        character(len=*), intent(inout) :: a, b
        character(len=len(a)) :: t
        t = a; a = b; b = t
    end subroutine

    subroutine s(c, d)
        character(len=*), intent(inout) :: c, d
        call g%swap(c, d)
    end subroutine
end module

program namespace_modules_30
    implicit none
end program
