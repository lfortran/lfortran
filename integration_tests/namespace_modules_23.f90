! A generic interface accessed through a module entity is not extended by a
! local generic interface of the same name: "swap" and "g%swap" are two
! independent generics. (With an ordinary USE, the local interface would
! extend the use-associated generic instead.)
module namespace_modules_23_gen
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

module namespace_modules_23_user
    use, namespace :: g => namespace_modules_23_gen
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

    subroutine both(i, j, c, d)
        integer, intent(inout) :: i, j
        character(len=*), intent(inout) :: c, d
        call g%swap(i, j)
        call swap(c, d)
    end subroutine
end module

program namespace_modules_23
    use namespace_modules_23_user, only: both
    implicit none
    integer :: i = 1, j = 2
    character(len=1) :: c = "a", d = "b"
    call both(i, j, c, d)
    if (i /= 2 .or. j /= 1) error stop
    if (c /= "b" .or. d /= "a") error stop
    print *, i, j, c, d
end program
