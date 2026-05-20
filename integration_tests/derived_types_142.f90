program derived_types_142
    implicit none

    type :: t
        integer, pointer :: p(:)
    end type t

    type(t) :: x, y
    integer, target :: arr(3)

    arr = [10, 20, 30]

    ! Associate y%p with arr
    y%p => arr

    ! Intrinsic assignment: pointer component gets pointer-assigned
    x = y

    ! Both x%p and y%p should be associated with arr
    if (x%p(1) /= 10) error stop
    if (x%p(2) /= 20) error stop
    if (x%p(3) /= 30) error stop
    if (y%p(1) /= 10) error stop
    if (y%p(2) /= 20) error stop
    if (y%p(3) /= 30) error stop

    ! Modify through x%p; y%p should see the change (same target)
    x%p(2) = 99
    if (y%p(2) /= 99) error stop
    if (arr(2) /= 99) error stop

    print *, "PASS"
end program derived_types_142
