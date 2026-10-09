module traits_runtime_pointer_null_01_oracle_types
    implicit none
    type :: A
    end type
    type, extends(A) :: Child
    end type
contains
    subroutine take_parent(p)
        class(A), pointer, intent(in) :: p
        if (associated(p)) error stop 1
    end subroutine
    subroutine readonly_mold(owner)
        class(Child), allocatable, intent(in) :: owner
        class(A), pointer :: p
        p => null(mold=owner)
        call take_parent(null(owner))
        if (associated(p)) error stop 7
    end subroutine
end module

module traits_runtime_pointer_null_01_oracle_aliases
    use traits_runtime_pointer_null_01_oracle_types, only: Parent => A, RenamedChild => Child
    implicit none
end module

program traits_runtime_pointer_null_01_oracle
    use traits_runtime_pointer_null_01_oracle_types
    use traits_runtime_pointer_null_01_oracle_aliases
    implicit none
    class(A), pointer :: p => null()
    class(Parent), pointer :: renamed => null()
    class(Child), pointer :: child_view => null()
    class(RenamedChild), pointer :: alias => null()
    class(Child), allocatable :: owner
    p => null()
    p => null(child_view)
    p => null(alias)
    p => null(owner)
    renamed => null(child_view)
    p => null(renamed)
    call take_parent(null(child_view))
    call take_parent(null(owner))
    call take_parent(null())
    if (associated(p) .or. associated(renamed) .or. associated(alias)) error stop 3
    if (allocated(owner)) error stop 6
    call readonly_mold(owner)
    if (allocated(owner)) error stop 8
    allocate(Child :: owner)
    call readonly_mold(owner)
    if (.not. allocated(owner)) error stop 9
    deallocate(owner)
end program
