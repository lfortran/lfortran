module traits_runtime_component_18_oracle_m
    implicit none
    type, abstract :: IUpdate
    contains
        procedure(update_interface), deferred :: update
    end type
    abstract interface
        subroutine update_interface(self)
            import IUpdate
            class(IUpdate), intent(inout) :: self
        end subroutine
    end interface
    type, extends(IUpdate) :: Coeffs
    contains
        procedure :: update => update_coeffs
    end type
    type, extends(IUpdate) :: Interp
    contains
        procedure :: update => update_interp
    end type
    type :: Polynoms
        class(IUpdate), allocatable :: coeffs, interp
    end type
    integer :: calls = 0, updates = 0
contains
    subroutine update_coeffs(self)
        class(Coeffs), intent(inout) :: self
        updates = updates + 1
    end subroutine
    subroutine update_interp(self)
        class(Interp), intent(inout) :: self
        updates = updates + 10
    end subroutine
    function make_polynoms(coeffs, interp) result(self)
        class(IUpdate), intent(in) :: coeffs, interp
        type(Polynoms) :: self
        calls = calls + 1
        self%coeffs = coeffs
        self%interp = interp
    end function
end module

program traits_runtime_component_18_oracle
    use traits_runtime_component_18_oracle_m
    implicit none
    type(Polynoms), allocatable :: p

    p = make_polynoms(Coeffs(), Interp())
    if (calls /= 1) error stop 1
    if (.not. allocated(p%coeffs) .or. .not. allocated(p%interp)) error stop 2
    call p%coeffs%update()
    call p%interp%update()
    if (updates /= 11) error stop 3
    p = make_polynoms(Interp(), Coeffs())
    if (calls /= 2) error stop 4
    call p%coeffs%update()
    if (updates /= 21) error stop 5
    call p%interp%update()
    if (updates /= 22) error stop 6
    deallocate(p)
end program
