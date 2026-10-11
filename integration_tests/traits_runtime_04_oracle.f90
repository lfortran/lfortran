module oracle_runtime_02_m
    implicit none
    integer :: a_finalizations = 0, b_finalizations = 0
    type, abstract :: ValueBase
    contains
        procedure(value_signature), deferred :: value
    end type ValueBase
    abstract interface
        function value_signature(self) result(r)
            import :: ValueBase
            class(ValueBase), intent(in) :: self
            integer :: r
        end function value_signature
    end interface
    type, extends(ValueBase) :: OwnedA
        integer, allocatable :: payload(:)
    contains
        procedure :: value => a_value
        final :: finish_a
    end type OwnedA
    type, extends(ValueBase) :: OwnedB
        integer :: payload
    contains
        procedure :: value => b_value
        final :: finish_b
    end type OwnedB
contains
    function a_value(self) result(r)
        class(OwnedA), intent(in) :: self
        integer :: r
        r = self%payload(1)
    end function a_value
    function b_value(self) result(r)
        class(OwnedB), intent(in) :: self
        integer :: r
        r = self%payload
    end function b_value
    subroutine finish_a(self)
        type(OwnedA), intent(inout) :: self
        a_finalizations = a_finalizations + 1
    end subroutine finish_a
    subroutine finish_b(self)
        type(OwnedB), intent(inout) :: self
        b_finalizations = b_finalizations + 1
    end subroutine finish_b
end module oracle_runtime_02_m

program oracle_runtime_02
    use oracle_runtime_02_m
    implicit none
    type(OwnedA) :: source_a
    type(OwnedB) :: source_b
    class(ValueBase), allocatable :: owner, copy
    integer :: before_a, before_b

    allocate(source_a%payload(1))
    source_a%payload(1) = 17
    source_b%payload = 29
    allocate(owner, source=source_a)
    if (owner%value() /= 17) error stop 901
    copy = owner
    source_a%payload(1) = 88
    if (owner%value() /= 17) error stop 902
    if (copy%value() /= 17) error stop 903

    before_a = a_finalizations
    before_b = b_finalizations
    deallocate(owner)
    if (a_finalizations /= before_a + 1) error stop 904
    if (b_finalizations /= before_b) error stop 905
    if (allocated(owner)) error stop 906
    if (copy%value() /= 17) error stop 907

    owner = source_a
    if (owner%value() /= 88) error stop 908
    owner = source_b
    if (owner%value() /= 29) error stop 909
    if (copy%value() /= 17) error stop 910

    before_a = a_finalizations
    before_b = b_finalizations
    deallocate(owner)
    if (a_finalizations /= before_a) error stop 911
    if (b_finalizations /= before_b + 1) error stop 912
    before_a = a_finalizations
    before_b = b_finalizations
    deallocate(copy)
    if (a_finalizations /= before_a + 1) error stop 913
    if (b_finalizations /= before_b) error stop 914
    if (allocated(copy)) error stop 915
    deallocate(source_a%payload)
end program oracle_runtime_02
