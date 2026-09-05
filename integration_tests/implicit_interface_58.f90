! A separately compilable external procedure with a CHARACTER(len=*) dummy is
! passed as an actual argument to a dummy procedure.
!
! An external procedure uses the classic Fortran external ABI (the CHARACTER
! data pointer at the argument position plus a hidden trailing length), while a
! dummy procedure must accept ordinary module and contained procedures too and
! therefore uses LFortran's string-descriptor ABI. Taking the address of
! `record_len` makes the two meet, so `record_len` has to be emitted with the
! descriptor ABI here. LFortran used to emit its definition with the external
! ABI regardless and then fail with "module failed verification: Call parameter
! type does not match function signature".
!
! See implicit_interface_57 for the same shape with a module procedure as the
! actual argument.
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
