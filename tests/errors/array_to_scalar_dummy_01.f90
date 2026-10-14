! An array passed to a scalar dummy argument.
module array_to_scalar_dummy_01_mod
    implicit none
    type, abstract :: AbsType
    end type AbsType
end module array_to_scalar_dummy_01_mod

subroutine array_to_scalar_dummy_01_integer()
    implicit none
    integer :: a(3)
    a = 1
    call scalar_dummy(a)  ! {Error} rank mismatch in argument `n`: the dummy argument is a scalar but the actual argument is an array of rank 1
contains
    subroutine scalar_dummy(n)
        integer, intent(in) :: n
        print *, n
    end subroutine
end subroutine

program array_to_scalar_dummy_01
    use array_to_scalar_dummy_01_mod
    implicit none
    type, extends(AbsType) :: MyType
    end type MyType
    class(AbsType), allocatable :: objs(:)
    allocate(MyType :: objs(2))
    call process(objs(1:1))  ! {Error} rank mismatch in argument `obj`: the dummy argument is a scalar but the actual argument is an array of rank 1
contains
    subroutine process(obj)
        class(AbsType), intent(in) :: obj
    end subroutine process
end program array_to_scalar_dummy_01
