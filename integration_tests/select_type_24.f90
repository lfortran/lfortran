module select_type_24_mod
    implicit none

    type, abstract :: AbsType
    contains
        procedure(add_ifc), deferred :: add
    end type AbsType

    type, extends(AbsType) :: MyType
        real(8), allocatable :: arr(:,:)
    contains
        procedure :: add => addition
    end type MyType

    abstract interface
        function add_ifc(a, b) result(r)
            import :: AbsType
            class(AbsType), intent(in)  :: a, b
            class(AbsType), allocatable :: r
        end function add_ifc
    end interface

contains

    function addition(a, b) result(r)
        class(MyType),  intent(in)  :: a
        class(AbsType), intent(in)  :: b
        class(AbsType), allocatable :: r

        select type (b)
        type is (MyType)
            allocate(MyType :: r)
            select type (r)
            type is (MyType)
                r%arr = a%arr + b%arr
            end select
        class default
            error stop "Unsupported RHS type"
        end select
    end function addition

end module select_type_24_mod


program select_type_24
    use select_type_24_mod
    implicit none

    type(MyType) :: a, b, c
    class(AbsType), allocatable :: r

    allocate(a%arr(2,2), b%arr(2,2))
    a%arr = reshape([1d0, 2d0, 3d0, 4d0], [2,2])
    b%arr = reshape([5d0, 6d0, 7d0, 8d0], [2,2])

    r = a%add(b)

    select type (r)
    type is (MyType)
        c = r
    class default
        error stop "add() returned wrong dynamic type"
    end select

    if (any(c%arr /= a%arr + b%arr)) then
        error stop "Incorrect addition result"
    end if

    print *, "a = ", a%arr
    print *, "b = ", b%arr
    print *, "c = ", c%arr

end program select_type_24
