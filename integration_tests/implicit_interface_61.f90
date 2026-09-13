! An external procedure and an unrelated dummy procedure may share a name.
! The dummy procedure is local and must not create a conflicting global symbol.
program implicit_interface_61
    implicit none
    integer :: n, k
    external :: report_len
    interface
        subroutine apply_op(op, k)
            interface
                subroutine op(x, y)
                    integer, intent(in) :: x
                    integer, intent(out) :: y
                end subroutine op
            end interface
            integer, intent(out) :: k
        end subroutine apply_op

        subroutine double_it(x, y)
            integer, intent(in) :: x
            integer, intent(out) :: y
        end subroutine double_it
    end interface

    call report_len('hello', n)
    if (n /= 5) error stop 1

    call apply_op(double_it, k)
    if (k /= 14) error stop 2

    print *, "OK"
end program implicit_interface_61
