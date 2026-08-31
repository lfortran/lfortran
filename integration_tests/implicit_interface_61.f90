! An external procedure with a CHARACTER(len=*) dummy is defined in a file that
! also declares, for an unrelated procedure, a DUMMY PROCEDURE of the same
! name.
!
! An external procedure uses the classic Fortran external ABI (the CHARACTER
! data pointer at the argument position plus a hidden trailing length), except
! when its address is taken, in which case it must keep LFortran's
! string-descriptor ABI so its pointer matches the dummy procedure it is bound
! to (see implicit_interface_58). That exclusion is keyed by procedure name,
! because an interface body and the definition it describes are two ASR symbols
! for one linked procedure.
!
! A dummy procedure is not an address-taken external procedure, so it must not
! be a key. When it was, the definition of `report_len` in the other file was
! emitted with the string-descriptor ABI while this file's call site used the
! external ABI, and the callee read a garbage length -- with no diagnostic,
! purely because of the name collision.
!
! Registered for gfortran only, and locked down for LFortran by the
! `external_abi` reference test on implicit_interface_61b.f90 in
! tests/tests.toml: LFortran cannot yet *link* this shape, because a dummy
! procedure emits a global LLVM declaration under its own name and a top-level
! subprogram of the same name is then renamed by LLVM. That is a separate,
! pre-existing bug with no CHARACTER involvement. Promote this test to `llvm`
! once it is fixed.
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
