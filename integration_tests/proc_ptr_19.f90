module proc_ptr_19_mod
    implicit none
    type :: runner
        procedure(), nopass, pointer :: caller => null()
    contains
        procedure, pass(this) :: set_caller
    end type
contains
    subroutine set_caller(this, caller)
        class(runner), intent(inout) :: this
        procedure() :: caller
        this%caller => caller
    end subroutine
end module

program proc_ptr_19
    use proc_ptr_19_mod
    implicit none
    type(runner) :: br
    integer :: val

    call br%set_caller(upper_caller)
    val = 42
    call br%caller(my_f, val)
contains
    subroutine upper_caller(f, a)
        procedure() :: f
        integer :: a
        call f(a)
    end subroutine
    
    subroutine my_f(a)
        integer :: a
        print *, "my_f called with", a
        if (a /= 42) error stop
    end subroutine
end program
