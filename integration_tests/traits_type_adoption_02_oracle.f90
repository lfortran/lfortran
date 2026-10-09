module traits_type_adoption_02_oracle_m
    implicit none
    private
    public :: Parent, Child, static_read, read_subset
    type, abstract :: Parent
        integer :: seed = 5
    contains
        procedure :: value => parent_value
        procedure(extra_interface), pass(self), deferred :: extra
        procedure(measure_interface), deferred :: measure
        procedure(legacy_interface), deferred :: legacy
    end type
    type, abstract, extends(Parent) :: Middle
        real(8) :: padding(3) = [1.0_8, 2.0_8, 3.0_8]
    end type
    type, extends(Middle) :: Child
        integer :: bonus = 11
    contains
        procedure, pass(self) :: extra => child_extra
        procedure :: measure => child_measure
        procedure :: legacy => child_legacy
    end type
    abstract interface
        integer function extra_interface(n, self)
            import Parent
            integer, intent(in) :: n
            class(Parent), intent(in) :: self
        end function
        real function measure_interface(self)
            import Parent
            class(Parent), intent(in) :: self
        end function
        integer function legacy_interface(self)
            import Parent
            class(Parent), intent(in) :: self
        end function
    end interface
contains
    integer function parent_value(self) result(n)
        class(Parent), intent(in) :: self
        n = self%seed
    end function
    integer function child_extra(n, self) result(v)
        integer, intent(in) :: n
        class(Child), intent(in) :: self
        v = n + self%seed + self%bonus
    end function
    real function child_measure(self) result(v)
        class(Child), intent(in) :: self
        v = real(self%seed + self%bonus)
    end function
    integer function child_legacy(self) result(v)
        class(Child), intent(in) :: self
        v = self%seed - 1
    end function
    integer function static_read(x) result(n)
        class(Parent), intent(in) :: x
        n = x%value() + x%extra(2) + int(x%measure())
    end function
    integer function read_subset(x) result(n)
        class(Parent), intent(in) :: x
        n = x%value() + x%extra(2)
    end function
end module

program traits_type_adoption_02_oracle
    use traits_type_adoption_02_oracle_m
    implicit none
    type(Child), target :: x
    class(Parent), pointer :: ordinary, view
    class(Parent), allocatable :: owner
    if (static_read(x) /= 39) error stop
    if (read_subset(x) /= 23) error stop
    ordinary => x
    if (ordinary%legacy() /= 4 .or. ordinary%value() /= 5) error stop
    view => x
    if (read_subset(view) /= 23) error stop
    allocate(owner, source=x)
    if (read_subset(owner) /= 23) error stop
    select type(owner)
    type is (Child)
        if (owner%bonus /= 11) error stop
    class default
        error stop
    end select
    x%seed = 8
    if (read_subset(view) /= 29 .or. read_subset(owner) /= 23) error stop
    nullify(view, ordinary)
    deallocate(owner)
    print *, "abstract adoption: 39 23 29; legacy: 4"
end program
