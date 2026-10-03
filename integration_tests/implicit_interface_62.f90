! Taking the address of a module procedure must not affect an unrelated
! external procedure with the same name in another compilation unit.
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
