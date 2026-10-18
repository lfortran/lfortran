module traits_runtime_allocatable_01_m
    implicit none
    integer :: orbits = 0
    abstract interface :: ISetup
        subroutine fill(self, data)
            type(itself), pass, intent(in) :: self
            real(8), allocatable, intent(out) :: data(:,:)
        end subroutine
        subroutine orbit(self, data)
            type(itself), pass, intent(in) :: self
            real(8), optional, allocatable, intent(out) :: data(:,:)
        end subroutine
        subroutine amend(self, data)
            type(itself), pass, intent(in) :: self
            real(8), allocatable, intent(inout) :: data(:,:)
        end subroutine
        subroutine reference(data)
            real(8), allocatable, intent(out) :: data(:,:)
        end subroutine
        integer function scaled(factor, self)
            type(itself), pass, intent(in) :: self
            integer, optional, intent(in) :: factor
        end function
    end interface
    type, implements(ISetup) :: Setup
        integer :: n = 7
    contains
        procedure :: fill
        procedure :: orbit
        procedure :: amend
        procedure, nopass :: reference
        procedure, pass(self) :: scaled
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
    integer function scaled(factor, self)
        class(Setup), intent(in) :: self
        integer, optional, intent(in) :: factor
        scaled = self%n
        if (present(factor)) scaled = scaled * factor
    end function
    subroutine forward(object, data)
        class(ISetup), intent(in) :: object
        real(8), optional, allocatable, intent(out) :: data(:,:)
        call object%orbit(data)
    end subroutine
end module

program traits_runtime_allocatable_01
    use traits_runtime_allocatable_01_m
    implicit none
    class(ISetup), allocatable :: object
    real(8), allocatable :: data(:,:)

    allocate(object, source=Setup())
    allocate(data(1,1))
    call object%fill(data)
    if (any(shape(data) /= [2,3]) .or. any(data /= 7._8)) error stop 3
    if (any(lbound(data) /= [-1,3])) error stop 4
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
    call object%amend(data)
    if (any(shape(data) /= [1,1]) .or. any(data /= 7._8)) error stop 11
    deallocate(data, object)
end program
