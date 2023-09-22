program arrays_34
    implicit none

    integer, pointer :: x(:)
    call sub(x, 1)
    print *, x
    if( any(x /= 1) ) error stop

contains

    subroutine sub(x, z)
        integer, intent(out), pointer :: x(:)
        integer, intent(in) :: z
        integer :: n

        select case (z)
            case (1)
                n = 1
                allocate(x(n))
                x = (/ 1 /)

            case default
                print *, "z =", z
                print *, "z not supported."
        end select
    end subroutine sub
end program
