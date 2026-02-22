module class_105_mod
    implicit none

    type :: AbsType
        integer :: value
    end type AbsType

    type :: MyType
        class(AbsType), allocatable :: arr(:)
    end type MyType

contains

    function tester() result(obj)
        class(MyType), allocatable :: obj
        obj = MyType()
        allocate(obj%arr(1))
        obj%arr(1)%value = 42
    end function tester

end module class_105_mod

program class_105
    use class_105_mod
    implicit none

    class(MyType), allocatable :: result_obj

    allocate(result_obj)
    result_obj = tester()

    if (.not. allocated(result_obj)) error stop "result_obj is not allocated"

    select type (result_obj)
    type is (MyType)
        if (.not. allocated(result_obj%arr)) error stop "result_obj%arr is not allocated"
        if (size(result_obj%arr) /= 1) error stop "result_obj%arr has wrong size"
        if (result_obj%arr(1)%value /= 42) error stop "result_obj%arr(1)%value has wrong value"
    class default
        error stop "result_obj has wrong dynamic type"
    end select
end program class_105
