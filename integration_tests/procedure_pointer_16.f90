program procedure_pointer_16
    implicit none
    type :: callback_holder
        procedure(), pointer, nopass :: ifunc => null()
    end type

    type(callback_holder) :: obj
    integer :: res

    obj%ifunc => isquare
    call obj%ifunc(6, res)
    if (res /= 36) error stop "FAIL: expected 36"

    obj%ifunc => icube
    call obj%ifunc(6, res)
    if (res /= 216) error stop "FAIL: expected 216"

    print *, "PASS"
contains
    subroutine isquare(n, out)
        integer, intent(in) :: n
        integer, intent(out) :: out
        out = n * n
    end subroutine
    subroutine icube(n, out)
        integer, intent(in) :: n
        integer, intent(out) :: out
        out = n ** 3
    end subroutine
end program
