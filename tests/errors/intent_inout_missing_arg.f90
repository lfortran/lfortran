module m1
    implicit none
    type :: runner
        procedure(), nopass, pointer :: caller => null()
    contains
    end type
contains
    subroutine set_caller(this, caller)
        class(runner), intent(inout) :: this
        procedure() :: caller
        this%caller => caller
    end subroutine
end module

program p
    use m1
    implicit none
    type(runner) :: br
    call set_caller(upper_caller)
contains
    subroutine upper_caller(a)
        class(*), intent(in) :: a
    end subroutine
end program
