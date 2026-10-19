! A retroactive conformance declared in a third module is found by a trait
! guard only where it is visible; the conformances declared in the module of
! a type are found everywhere (TD-11).
module traits_runtime_conformance_query_04_contracts
    implicit none
    abstract interface :: ILabel
        integer function label(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface, extends(ILabel) :: IRich
        integer function rich(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
end module

module traits_runtime_conformance_query_04_types
    use traits_runtime_conformance_query_04_contracts
    implicit none
    type :: Shared
        integer :: n = 0
    end type
    type :: Child
        integer :: n = 0
    end type
    type :: Owned
        integer :: n = 0
    end type
    implements ILabel :: Owned
        procedure, pass :: label => owned_label
    end implements
contains
    integer function owned_label(self)
        type(Owned), intent(in) :: self
        owned_label = 100 + self%n
    end function
end module

module traits_runtime_conformance_query_04_retro
    use traits_runtime_conformance_query_04_contracts
    use traits_runtime_conformance_query_04_types, only: Shared, Child
    implicit none
    implements ILabel :: Shared
        procedure, pass :: label => shared_label
    end implements
    implements IRich :: Child
        procedure, pass :: label => child_label
        procedure, pass :: rich => child_rich
    end implements
contains
    integer function shared_label(self)
        type(Shared), intent(in) :: self
        shared_label = 200 + self%n
    end function
    integer function child_label(self)
        type(Child), intent(in) :: self
        child_label = 300 + self%n
    end function
    integer function child_rich(self)
        type(Child), intent(in) :: self
        child_rich = 400 + self%n
    end function
end module

module traits_runtime_conformance_query_04_blind
    use traits_runtime_conformance_query_04_contracts
    implicit none
contains
    integer function blind_label(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (ILabel)
            r = x%label()
        class default
            r = -1
        end select
    end function
    ! A trait object always conforms to the traits its contract implies, with
    ! the conformance it was formed with, here one this module cannot see.
    integer function blind_view(view) result(r)
        class(ILabel), intent(in) :: view
        select type (x => view)
        class is (IRich)
            r = x%rich()
        class is (ILabel)
            r = x%label()
        end select
    end function
end module

module traits_runtime_conformance_query_04_seeing
    use traits_runtime_conformance_query_04_contracts
    use traits_runtime_conformance_query_04_retro
    implicit none
contains
    integer function seeing_label(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (ILabel)
            r = x%label()
        class default
            r = -1
        end select
    end function
    integer function seeing_rich(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (ILabel)
            r = x%label()
        class is (IRich)
            r = x%rich()
        class default
            r = -1
        end select
    end function
end module

program traits_runtime_conformance_query_04
    use traits_runtime_conformance_query_04_types
    use traits_runtime_conformance_query_04_blind
    use traits_runtime_conformance_query_04_seeing
    use traits_runtime_conformance_query_04_retro
    implicit none
    type(Shared) :: shared_value
    type(Child) :: child_value
    type(Owned) :: owned_value
    shared_value%n = 1
    child_value%n = 2
    owned_value%n = 3
    if (blind_label(shared_value) /= -1 .or. blind_label(child_value) /= -1) error stop 1
    if (blind_label(owned_value) /= 103) error stop 2
    if (seeing_label(shared_value) /= 201 .or. seeing_label(child_value) /= 302) error stop 3
    if (seeing_label(owned_value) /= 103 .or. seeing_label(7) /= -1) error stop 4
    if (seeing_rich(child_value) /= 402 .or. seeing_rich(shared_value) /= 201) error stop 5
    if (seeing_rich(owned_value) /= 103) error stop 6
    if (blind_view(shared_value) /= 201 .or. blind_view(child_value) /= 302) error stop 7
    if (blind_view(owned_value) /= 103) error stop 8
    print *, "trait guards: visible retroactive conformances"
end program
