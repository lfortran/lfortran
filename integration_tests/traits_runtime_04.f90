module traits_runtime_04_m
    implicit none
    integer :: a_finalizations = 0, b_finalizations = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
    type :: OwnedA
        integer, allocatable :: payload(:)
    contains
        final :: finish_a
    end type OwnedA
    type :: OwnedB
        integer :: payload
    contains
        final :: finish_b
    end type OwnedB
    implements IValue :: OwnedA
        procedure, pass :: value => a_value
    end implements OwnedA
    implements IValue :: OwnedB
        procedure, pass :: value => b_value
    end implements OwnedB
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
end module traits_runtime_04_m

program traits_runtime_04
    use traits_runtime_04_m
    implicit none
    type(OwnedA) :: source_a
    type(OwnedB) :: source_b
    class(IValue), allocatable :: owner, copy
    integer :: before_a, before_b

    allocate(source_a%payload(1))
    source_a%payload(1) = 17
    source_b%payload = 29
    allocate(owner, source=source_a)
    if (owner%value() /= 17) error stop 401
    copy = owner
    source_a%payload(1) = 88
    if (owner%value() /= 17) error stop 402
    if (copy%value() /= 17) error stop 403

    before_a = a_finalizations
    before_b = b_finalizations
    deallocate(owner)
    if (a_finalizations /= before_a + 1) error stop 404
    if (b_finalizations /= before_b) error stop 405
    if (allocated(owner)) error stop 406
    if (copy%value() /= 17) error stop 407

    owner = source_a
    if (owner%value() /= 88) error stop 408
    owner = source_b
    if (owner%value() /= 29) error stop 409
    if (copy%value() /= 17) error stop 410

    ! Count only stable, explicit ownership boundaries, not assignment temporaries.
    before_a = a_finalizations
    before_b = b_finalizations
    deallocate(owner)
    if (a_finalizations /= before_a) error stop 411
    if (b_finalizations /= before_b + 1) error stop 412
    before_a = a_finalizations
    before_b = b_finalizations
    deallocate(copy)
    if (a_finalizations /= before_a + 1) error stop 413
    if (b_finalizations /= before_b) error stop 414
    if (allocated(copy)) error stop 415
    deallocate(source_a%payload)
end program traits_runtime_04
