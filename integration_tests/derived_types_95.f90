program derived_types_95
    type:: tt
        integer :: n
    end type

    class(tt), allocatable :: arr(:)
    integer :: i

    print *, allocated(arr)
    if(allocated(arr)) error stop
    
    allocate(arr(3))
    arr(1)%n = 10
    arr(2)%n = 20
    arr(3)%n = 30

    do i = 1, size(arr)
        print *, arr(i)%n
        if(arr(i)%n /= i*10) error stop
    end do

    print *, allocated(arr)
    if(.not. allocated(arr)) error stop

    call foo(arr)
contains 
    subroutine foo(arg)
        class(tt), intent(in) :: arg(:)
        integer :: i
        print *, size(arg)
        if(size(arg) /= 3) error stop
        do i = 1, size(arg)
            print *, arg(i)%n
            ! if(arg(i)%n /= i*10) error stop >>>>>>>>>>>>>>>> !TODO: Uncomment when fixed
        end do
    end subroutine
end program