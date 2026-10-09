module traits_type_adoption_03_oracle_m
    implicit none
    type :: Box
        integer :: n = 17
    contains
        procedure, pass(self) :: value
        procedure, pass(self) :: add
    end type
contains
    integer function value(delta, self)
        integer, intent(in) :: delta
        class(Box), intent(in) :: self
        value = self%n + delta
    end function
    subroutine add(n, self)
        integer, intent(in) :: n
        class(Box), intent(inout) :: self
        self%n = self%n + n
    end subroutine
    subroutine exercise(view)
        class(Box), intent(inout) :: view
        if (view%value(1) /= 18) error stop 1
        call view%add(n=3)
        if (view%value(delta=2) /= 22) error stop 2
    end subroutine
end module

program traits_type_adoption_03_oracle
    use traits_type_adoption_03_oracle_m
    implicit none
    type(Box), target :: object
    class(Box), pointer :: view
    class(Box), allocatable :: owner
    view => object
    call exercise(view)
    if (object%n /= 20) error stop 3
    allocate(owner)
    call exercise(owner)
    if (owner%value(2) /= 22) error stop 4
    deallocate(owner)
    nullify(view)
end program
