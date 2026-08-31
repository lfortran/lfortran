! An external procedure with a CHARACTER(len=*) dummy is defined in a file that
! also contains a MODULE PROCEDURE of the same name whose address is taken.
!
! Taking the address of an external procedure makes it keep LFortran's
! string-descriptor ABI (see implicit_interface_58), and that exclusion is
! keyed by procedure name. A module procedure never uses the external ABI in
! the first place -- it is even emitted under a mangled link name -- so it must
! not be a key. When it was, the external `report_len` in the other file was
! emitted with the string-descriptor ABI while this file's call site used the
! external ABI, and the callee read a garbage length.
!
! This program deliberately does not USE the module: the collision has to be
! observed from a translation unit that knows nothing about it, so that the two
! sides of the call can disagree.
!
! See implicit_interface_61 for the same collision through a dummy procedure.
program implicit_interface_62
    implicit none
    integer :: n, k
    external :: report_len, run_helper

    call report_len('hello', n)
    if (n /= 5) error stop 1

    call run_helper(k)
    if (k /= 14) error stop 2

    print *, "OK"
end program implicit_interface_62
