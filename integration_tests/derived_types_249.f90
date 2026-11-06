! A type with an allocatable array component of its own type passed to a
! class(t) dummy (#16523)
module derived_types_249_m
    implicit none
    type :: t
        integer :: k = 0
        type(t), allocatable :: items(:)
    end type t
contains
    recursive subroutine f(a)
        class(t), intent(inout) :: a
        a%k = a%k + 1
        if (allocated(a%items)) call f(a%items(1))
    end subroutine f
end module derived_types_249_m

program derived_types_249
    use derived_types_249_m
    implicit none
    type(t) :: x
    allocate(x%items(1))
    allocate(x%items(1)%items(2))
    call f(x)
    print *, x%k, x%items(1)%k, x%items(1)%items(1)%k
    if (x%k /= 1) error stop 1
    if (x%items(1)%k /= 1) error stop 2
    if (x%items(1)%items(1)%k /= 1) error stop 3
    if (x%items(1)%items(2)%k /= 0) error stop 4
    if (allocated(x%items(1)%items(1)%items)) error stop 5
end program derived_types_249
