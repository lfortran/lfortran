module overload_assignment_m
    implicit none
    private
    public assignment (=)

    interface assignment (=)
        module procedure logical_gets_integer
    end interface
contains
    subroutine logical_gets_integer(tf, i)
        logical, intent (out) :: tf
        integer, intent (in)  :: i

        tf = (i == 0)
    end subroutine

    subroutine logical_gets_integer_use(tf, i)
        logical, intent (out) :: tf
        integer, intent (in)  :: i

        tf = i
    end subroutine
end module

program main
    use overload_assignment_m, only: assignment(=)
    implicit none
    logical :: tf

    tf = 0
    print *, "tf=0:", tf  ! Yields: T
    tf = 1
    print *, "tf=1:", tf  ! Yields: F
end program
