module derived_types_82_module
    implicit none

    type :: shlex_token
        character(len=:), allocatable :: text
    contains
        procedure :: neq
        generic :: operator(/=) => neq
    end type shlex_token

contains

    logical function neq(a, b)
        class(shlex_token), intent(in) :: a, b
        if (.not.allocated(a%text) .or. .not.allocated(b%text)) then
            neq = allocated(a%text) .neqv. allocated(b%text)
        else
            neq = (a%text /= b%text)
        end if
    end function neq

end module derived_types_82_module


program derived_types_82
    use derived_types_82_module
    implicit none

    type(shlex_token), allocatable :: list(:), lmsvcrt(:)
    integer :: i

    allocate(list(3), lmsvcrt(3))

    list(1)%text = "foo"
    list(2)%text = "bar"
    list(3)%text = "baz"

    lmsvcrt(1)%text = "foo"
    lmsvcrt(2)%text = "BAR"
    lmsvcrt(3)%text = "baz"

    if (list(1) /= lmsvcrt(1)) error stop
    if (.not.(list(2) /= lmsvcrt(2))) error stop
    if (list(3) /= lmsvcrt(3)) error stop
end program derived_types_82