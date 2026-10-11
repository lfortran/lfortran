module traits_runtime_owning_01_oracle_m
    implicit none
    integer :: final_values = 0, final_parts = 0
    type, abstract :: ValueBase
    contains
        procedure(value_signature), deferred :: value
    end type
    abstract interface
        function value_signature(self) result(r)
            import :: ValueBase
            class(ValueBase), intent(in) :: self
            integer :: r
        end function
    end interface
    type :: Part
        integer :: n = 7
    contains
        final :: finish_part
    end type
    type, extends(ValueBase) :: Data
        integer :: n = 5
        type(Part) :: part
        integer, allocatable :: data(:)
        integer, pointer :: link => null()
    contains
        procedure :: value => read_data
        final :: finish_data
    end type
    class(ValueBase), allocatable :: module_owner
contains
    function read_data(self) result(r)
        class(Data), intent(in) :: self
        integer :: r
        r = self%n + self%part%n
        if (allocated(self%data)) r = r + sum(self%data)
        if (associated(self%link)) r = r + self%link
    end function
    subroutine finish_part(self)
        type(Part), intent(inout) :: self
        final_parts = final_parts + 1
    end subroutine
    subroutine finish_data(self)
        type(Data), intent(inout) :: self
        final_values = final_values + 1
    end subroutine
    function observe(view) result(r)
        class(ValueBase), intent(in) :: view
        integer :: r
        r = view%value()
    end function
    subroutine copy_view(view)
        class(ValueBase), intent(in) :: view
        module_owner = view
    end subroutine
    subroutine local_lifetime(source, early)
        type(Data), intent(in) :: source
        logical, intent(in) :: early
        class(ValueBase), allocatable :: local
        allocate(local, source=source)
        if (observe(local) /= read_data(source)) error stop 1
        if (early) return
    end subroutine
end module

program traits_runtime_owning_01_oracle
    use traits_runtime_owning_01_oracle_m
    implicit none
    type(Data) :: source
    class(ValueBase), allocatable :: owner, copy
    integer, target :: target
    integer :: before_values, before_parts, i

    if (allocated(owner)) error stop 2
    if (allocated(module_owner)) error stop 3
    allocate(Data :: owner)
    if (owner%value() /= 12) error stop 4
    deallocate(owner)
    source%n = 31
    source%part%n = 9
    allocate(source%data(3))
    source%data = [10, 20, 30]
    target = 100
    source%link => target
    before_values = final_values
    before_parts = final_parts
    allocate(owner, source=source)
    if (final_values /= before_values .or. final_parts /= before_parts) error stop 5
    if (owner%value() /= 200) error stop 6
    allocate(copy, mold=owner)
    if (copy%value() /= 12) error stop 7
    deallocate(copy)
    allocate(copy, mold=source)
    if (copy%value() /= 12) error stop 8
    deallocate(copy)
    allocate(copy, source=owner)
    source%data = 99
    source%n = 99
    source%part%n = 99
    target = 200
    if (observe(owner) /= 300 .or. observe(copy) /= 300) error stop 9
    ! GFortran 16 corrupts the allocated component in the polymorphic self-copy
    ! variant. The extension test retains owner = owner; this oracle checks a
    ! distinct, equal source rather than encoding that compiler defect.
    owner = copy
    if (owner%value() /= 300) error stop 10
    call copy_view(owner)
    deallocate(owner)
    if (module_owner%value() /= 300 .or. copy%value() /= 300) error stop 11
    deallocate(module_owner, copy)
    if (allocated(module_owner) .or. allocated(copy)) error stop 12

    do i = 1, 2
        before_values = final_values
        before_parts = final_parts
        call local_lifetime(source, i == 1)
        if (final_values /= before_values + 1) error stop 13
        if (final_parts /= before_parts + 1) error stop 14
    end do
    before_values = final_values
    before_parts = final_parts
    block
        class(ValueBase), allocatable :: scoped
        scoped = source
        if (observe(scoped) /= 695) error stop 15
    end block
    if (final_values /= before_values + 1) error stop 16
    if (final_parts /= before_parts + 1) error stop 17
    if (target /= 200) error stop 18
    deallocate(source%data)
end program
