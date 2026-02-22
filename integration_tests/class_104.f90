module class_104_mod
    implicit none

    type, public, abstract :: AbsType
    end type AbsType

    type, extends(AbsType) :: MyType
      integer :: key
    end type MyType

    interface MyType
        procedure :: init
    end interface MyType

    type :: ClientType
    contains
        procedure :: client
    end type ClientType

contains

    function init() result(self)
        type(MyType) :: self
        self%key = 100
    end function init

    subroutine client(self, arr)
        class(ClientType),           intent(in)  :: self
        class(AbsType), allocatable, intent(out) :: arr(:)

        allocate(MyType :: arr(2))
        arr = MyType()
    end subroutine client

end module class_104_mod


program class_104
    use class_104_mod
    implicit none

    type(ClientType) :: c
    class(AbsType), allocatable :: arr(:)

    call c%client(arr)

    if (.not. allocated(arr)) error stop "arr not allocated"

    if (size(arr) /= 2) error stop "wrong array size"

    select type (arr)
    type is (MyType)
        print *, "arr(1)%key = ", arr(1)%key
        print *, "arr(2)%key = ", arr(2)%key
        if (arr(1)%key /= 100 .or. arr(2)%key /= 100) error stop "wrong key value"
    class default
        error stop "wrong dynamic type"
    end select

end program class_104
