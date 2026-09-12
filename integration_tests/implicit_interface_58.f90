! An external procedure passed as a dummy procedure uses the same CHARACTER
! descriptor as a module procedure (implicit_interface_57).
subroutine record_len(msg, n)
    implicit none
    character(len=*), intent(in) :: msg
    integer, intent(out) :: n
    n = len(msg)
    if (msg /= 'hello') n = -1
end subroutine record_len

subroutine driver_58(cb, n)
    implicit none
    interface
        subroutine cb(msg, n)
            character(len=*), intent(in) :: msg
            integer, intent(out) :: n
        end subroutine cb
    end interface
    integer, intent(out) :: n
    call cb('hello', n)
end subroutine driver_58

program implicit_interface_58
    implicit none
    interface
        subroutine record_len(msg, n)
            character(len=*), intent(in) :: msg
            integer, intent(out) :: n
        end subroutine record_len

        subroutine driver_58(cb, n)
            interface
                subroutine cb(msg, n)
                    character(len=*), intent(in) :: msg
                    integer, intent(out) :: n
                end subroutine cb
            end interface
            integer, intent(out) :: n
        end subroutine driver_58
    end interface
    integer :: n
    n = 0
    call driver_58(record_len, n)
    print *, "len seen by callback =", n, " (expected 5)"
    if (n /= 5) error stop
    print *, "OK"
end program implicit_interface_58
