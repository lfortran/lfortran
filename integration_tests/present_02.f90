program present_02
implicit none

type X

    integer :: stat = 77

end type

type(X), allocatable :: objx

print *, allocated(objx)
if( allocated(objx) ) error stop

call f(objx)

contains

    subroutine f(obj)
        type(X), allocatable, optional, intent(in) :: obj
        print *, present(obj)
        if( .not. present(obj) ) error stop
        call g(obj)
    end subroutine

    subroutine g(obj1)
        type(X), optional, intent(in), allocatable :: obj1
        print *, present(obj1)
        if( .not. present(obj1) ) error stop
        call h(obj1)
    end subroutine

    subroutine h(obj2)
        type(X), intent(in), optional :: obj2
        print *, present(obj2)
        if( present(obj2) ) error stop
    end subroutine

end program
