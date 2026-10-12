module class_158_mod
    implicit none

    type :: comm_t
        integer :: n = 0
    contains
        procedure :: abort
        procedure :: random_number => comm_random_number
    end type

    type(comm_t) :: comm

contains

    subroutine abort(this, message)
        class(comm_t), intent(inout) :: this
        character(*), intent(in) :: message
        this%n = this%n + len(message)
    end subroutine

    subroutine comm_random_number(this, x)
        class(comm_t), intent(inout) :: this
        integer, intent(in) :: x
        this%n = this%n + x
    end subroutine

end module

program class_158
    use class_158_mod, only: comm
    implicit none

    call comm%abort('message')
    if (comm%n /= 7) error stop
    call comm%random_number(10)
    if (comm%n /= 17) error stop
    print *, comm%n
end program
