program derived_types_92
    implicit none
    type :: package_t
        integer :: num
        character(:),allocatable :: str
    end type package_t

    class(package_t), allocatable :: instance(:)

    allocate(instance(1))
    instance(1)%num = 1
    instance(1)%str = "Hi"

    print *, instance(1)%num
    if(instance(1)%num /= 1) error stop
    
    print *, instance(1)%str
    if(instance(1)%str /= "Hi") error stop
    
    call foo(instance)

    contains
    subroutine foo(arg)
        class(package_t), allocatable :: arg(:)
        print *, arg(1)%num
        if(arg(1)%num /= 1) error stop
    
        print *, arg(1)%str
        if(arg(1)%str /= "Hi") error stop
    end subroutine 
end program 