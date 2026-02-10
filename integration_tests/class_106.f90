program test
    use class_106b_mod
    use class_106a_mod, only : MyType
    implicit none

    type(Client) :: c
    class(MyType), allocatable :: arr(:)

    call c%method(arr)

    if (.not. allocated(arr)) then
        error stop "arr not allocated"
    end if

    print *, "Test passed"
end program test
