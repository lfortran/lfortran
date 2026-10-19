! A retroactive conformance of the public type Shared, found by a trait guard
! only where this module is visible.
module traits_runtime_conformance_query_separate_01_retro
    use traits_runtime_conformance_query_separate_01_contracts, only: ILabel, Shared
    implicit none
    implements ILabel :: Shared
        procedure, pass :: label => shared_label
    end implements
contains
    integer function shared_label(self)
        type(Shared), intent(in) :: self
        shared_label = 40 + self%n
    end function
end module

module traits_runtime_conformance_query_separate_01_seeing
    use traits_runtime_conformance_query_separate_01_contracts
    use traits_runtime_conformance_query_separate_01_retro
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
end module
