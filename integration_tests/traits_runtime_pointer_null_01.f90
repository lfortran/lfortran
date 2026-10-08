module traits_runtime_pointer_null_01_contracts
    implicit none
    abstract interface :: A
    end interface
    abstract interface :: B
    end interface
    abstract interface, extends(A) :: Child
    end interface
    abstract interface, extends(A + B) :: Combined
    end interface
contains
    subroutine take_parent(p)
        class(A), pointer, intent(in) :: p
        if (associated(p)) error stop 1
    end subroutine
    subroutine take_combination(p)
        class(A + B), pointer, intent(in) :: p
        if (associated(p)) error stop 2
    end subroutine
end module

module traits_runtime_pointer_null_01_aliases
    use traits_runtime_pointer_null_01_contracts, only: Parent => A, RenamedChild => Child
    implicit none
end module

program traits_runtime_pointer_null_01
    use traits_runtime_pointer_null_01_contracts
    use traits_runtime_pointer_null_01_aliases
    implicit none
    class(A), pointer :: p => null()
    class(Parent), pointer :: renamed => null()
    class(Child), pointer :: child_view => null()
    class(RenamedChild), pointer :: alias => null()
    class(Combined), pointer :: combined_view => null()
    class(A + B), pointer :: ab => null()
    class(B + A), pointer :: ba => null()
    class(Child), allocatable :: owner
    p => null()
    p => null(child_view)
    p => null(alias)
    p => null(owner)
    renamed => null(child_view)
    p => null(renamed)
    p => null(ab)
    ab => null(ba)
    ab => null(combined_view)
    ba => null(ab)
    call take_parent(null(child_view))
    call take_parent(null(owner))
    call take_parent(null())
    call take_combination(null(ba))
    call take_combination(null(combined_view))
    if (associated(p) .or. associated(renamed) .or. associated(alias)) error stop 3
    if (associated(ab) .or. associated(ba) .or. associated(combined_view)) error stop 4
    if (allocated(owner)) error stop 6
end program
