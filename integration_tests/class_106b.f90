module class_106b_mod
    use class_106a_mod, only : AbsType, MyType, ConcreteType
    implicit none
    private

    type, public :: Client
    contains
        procedure :: method
    end type Client

contains

    subroutine method(self, arr)
        class(Client),              intent(in)  :: self
        class(MyType), allocatable, intent(out) :: arr(:)

        integer :: l, n, nd
        integer, allocatable        :: ints(:)
        class(AbsType), allocatable :: obj

        nd = 3

        allocate(ints(2))
        ints = [1, 2]

        allocate(ConcreteType :: obj)
        allocate(MyType :: arr(nd))

        arr = MyType(ints, obj)

        do l = nd-1, 1, -1
            do n = 1, nd-l
                call arr(l)%my_method()
            end do
        end do

        if (.not. allocated(arr)) error stop "arr not allocated"
        if (size(arr) /= nd) error stop "wrong arr size"

    end subroutine method

end module class_106b_mod
