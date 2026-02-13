module class_109_module
    implicit none

    type :: base_type
        integer :: k
    contains
        procedure :: nelements
    end type base_type

    type :: wrapper
        class(base_type), allocatable :: obj
    contains
        procedure :: caller
    end type wrapper

contains

    pure function nelements(self, i) result(n)
        class(base_type), intent(in) :: self
        integer, intent(in) :: i
        integer :: n
        n = self%k
    end function nelements

    subroutine caller(self)
        class(wrapper), intent(in) :: self
        real(8), dimension(self%obj%nelements(20)) :: a
        if (size(a) /= self%obj%k) error stop 1
    end subroutine caller

end module class_109_module

module class_109_abstract_module
    implicit none

    type, abstract :: abstract_type
    contains
        procedure(nelements_abstract_iface), deferred :: nelements
    end type abstract_type

    abstract interface
        pure function nelements_abstract_iface(self, i) result(n)
            import :: abstract_type
            class(abstract_type), intent(in) :: self
            integer, intent(in) :: i
            integer :: n
        end function nelements_abstract_iface
    end interface

    type, extends(abstract_type) :: concrete_type
        integer :: k
    contains
        procedure :: nelements => concrete_nelements
    end type concrete_type

    type :: wrapper_abstract
        class(abstract_type), allocatable :: obj
    contains
        procedure :: caller_abstract
    end type wrapper_abstract

contains

    pure function concrete_nelements(self, i) result(n)
        class(concrete_type), intent(in) :: self
        integer, intent(in) :: i
        integer :: n
        n = self%k
    end function concrete_nelements

    subroutine caller_abstract(self)
        class(wrapper_abstract), intent(in) :: self
        real(8), dimension(self%obj%nelements(20)) :: a
        if (size(a) /= 5) error stop 2
    end subroutine caller_abstract

end module class_109_abstract_module

program class_109
    use class_109_module
    use class_109_abstract_module
    implicit none

    type(wrapper) :: w
    type(wrapper_abstract) :: wa

    allocate(base_type :: w%obj)
    w%obj%k = 4

    call w%caller()

    allocate(concrete_type :: wa%obj)
    select type (obj => wa%obj)
    type is (concrete_type)
        obj%k = 5
    class default
        error stop 3
    end select

    call wa%caller_abstract()

    print *, "ok"
end program class_109
