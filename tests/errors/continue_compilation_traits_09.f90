! A type that is not generic extends an instance of a generic type only in a
! module so far (TD-45).
module traits_instance_extends_base
    implicit none
    abstract interface :: ISum
        integer function total(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    type, implements(ISum) :: Leaf
    contains
        procedure :: total => leaf_total
    end type
contains
    integer function leaf_total(self)
        class(Leaf), intent(in) :: self
        leaf_total = 1
    end function
end module

module traits_instance_extends_simple
    use traits_instance_extends_base, only: ISum
    implicit none
    type :: SimpleSum{ISum :: U}
        type(U), allocatable :: basic
    end type
end module

program traits_instance_extends_program
    use traits_instance_extends_base, only: Leaf
    use traits_instance_extends_simple, only: SimpleSum
    implicit none
    type, extends(SimpleSum{Leaf}) :: Local
    end type
    print *, 1
end program
