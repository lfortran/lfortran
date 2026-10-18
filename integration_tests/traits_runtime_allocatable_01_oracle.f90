module traits_runtime_allocatable_01_oracle_m
    implicit none
    integer :: orbits = 0
    type, abstract :: ISetup
    contains
        procedure(fill_interface), deferred :: fill
        procedure(orbit_interface), deferred :: orbit
        procedure(amend_interface), deferred :: amend
        procedure(reference_interface), deferred, nopass :: reference
        procedure(scaled_interface), deferred :: scaled
        procedure(count_interface), deferred :: count
    end type
    abstract interface
        subroutine fill_interface(self, data)
            import ISetup
            class(ISetup), intent(in) :: self
            real(8), allocatable, intent(out) :: data(:,:)
        end subroutine
        subroutine orbit_interface(self, data)
            import ISetup
            class(ISetup), intent(in) :: self
            real(8), optional, allocatable, intent(out) :: data(:,:)
        end subroutine
        subroutine amend_interface(self, data)
            import ISetup
            class(ISetup), intent(in) :: self
            real(8), allocatable, intent(inout) :: data(:,:)
        end subroutine
        subroutine reference_interface(data)
            real(8), allocatable, intent(out) :: data(:,:)
        end subroutine
        integer function scaled_interface(self, factor)
            import ISetup
            class(ISetup), intent(in) :: self
            integer, optional, intent(in) :: factor
        end function
        integer function count_interface(self, data)
            import ISetup
            class(ISetup), intent(in) :: self
            real(8), allocatable, intent(in) :: data(:,:)
        end function
    end interface
    type, extends(ISetup) :: Setup
        integer :: n = 7
    contains
        procedure :: fill
        procedure :: orbit
        procedure :: amend
        procedure, nopass :: reference
        procedure :: scaled
        procedure :: count
    end type
contains
    subroutine fill(self, data)
        class(Setup), intent(in) :: self
        real(8), allocatable, intent(out) :: data(:,:)
        if (allocated(data)) error stop 1
        allocate(data(-1:0,3:5))
        data = self%n
    end subroutine
    subroutine orbit(self, data)
        class(Setup), intent(in) :: self
        real(8), optional, allocatable, intent(out) :: data(:,:)
        orbits = orbits + 1
        if (present(data)) call self%fill(data)
    end subroutine
    subroutine amend(self, data)
        class(Setup), intent(in) :: self
        real(8), allocatable, intent(inout) :: data(:,:)
        if (.not. allocated(data)) allocate(data(1,1), source=0._8)
        data = data + self%n
    end subroutine
    subroutine reference(data)
        real(8), allocatable, intent(out) :: data(:,:)
        if (allocated(data)) error stop 2
        allocate(data(2,3))
        data = 13._8
    end subroutine
    integer function scaled(self, factor)
        class(Setup), intent(in) :: self
        integer, optional, intent(in) :: factor
        scaled = self%n
        if (present(factor)) scaled = scaled * factor
    end function
    integer function count(self, data)
        class(Setup), intent(in) :: self
        real(8), allocatable, intent(in) :: data(:,:)
        count = 0
        if (allocated(data)) count = size(data)
    end function
    subroutine forward(object, data)
        class(ISetup), intent(in) :: object
        real(8), optional, allocatable, intent(out) :: data(:,:)
        call object%orbit(data)
    end subroutine
end module

program traits_runtime_allocatable_01_oracle
    use traits_runtime_allocatable_01_oracle_m
    implicit none
    class(ISetup), allocatable :: object
    real(8), allocatable :: data(:,:)

    allocate(object, source=Setup())
    allocate(data(1,1))
    call object%fill(data)
    if (any(shape(data) /= [2,3]) .or. any(data /= 7._8)) error stop 3
    if (any(lbound(data) /= [-1,3])) error stop 4
    if (object%count(data) /= 6) error stop 12
    call object%orbit()
    call forward(object)
    if (orbits /= 2 .or. any(data /= 7._8)) error stop 5
    call forward(object, data)
    if (orbits /= 3 .or. any(data /= 7._8)) error stop 6
    call object%amend(data)
    if (any(data /= 14._8)) error stop 7
    call object%reference(data)
    if (any(data /= 13._8)) error stop 8
    if (object%scaled() /= 7) error stop 9
    if (object%scaled(factor=3) /= 21) error stop 10
    deallocate(data)
    if (object%count(data) /= 0) error stop 13
    call object%amend(data)
    if (any(shape(data) /= [1,1]) .or. any(data /= 7._8)) error stop 11
    deallocate(data, object)
end program
