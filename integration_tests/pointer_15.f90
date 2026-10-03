program pointer_15
    implicit none

    type :: base
        integer :: v
    end type

    class(base), pointer :: y
    class(base), pointer :: mmm
    call ss(y, 1000)
    call ss(mmm, 2222)

    print *, y%v
    if(y%v /= 1000) error stop

    print *, mmm%v
    if(mmm%v /= 2222) error stop
    
    deallocate(y)
    deallocate(mmm)

    contains
    subroutine ss(xxx, inn)
        integer :: inn
        class(base), pointer :: xxx
        type(base), pointer :: t
        allocate(t)
        t%v = inn
        xxx => t
    end subroutine 
end program 