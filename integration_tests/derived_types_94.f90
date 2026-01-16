! Test casting an array from C-Struct Type INTO Fortran Class {VTable +CStruct}
program derived_types_94
    implicit none
    type :: tt
        integer :: n
    end type tt
    type(tt), allocatable :: arr(:)
    allocate(arr(4))
    arr(1)%n = 1
    arr(2)%n = 2
    arr(3)%n = 3
    arr(4)%n = 4

    call foo(arr)
    call foo2(arr)
    contains 
    subroutine foo(arg)
        class(tt), intent(in) :: arg(:)
        integer :: i
        do i = 1, size(arg)
            write(*,*) "Package Name: ", arg(i)%n
            if(arg(i)%n /= i) error stop
        end do
    end subroutine
    
    subroutine foo2(arg)
        class(tt), intent(in) :: arg(4)
        integer :: i
        do i = 1, size(arg)
            write(*,*) "Package Name: ", arg(i)%n
            if(arg(i)%n /= i) error stop
        end do
    end subroutine
end program