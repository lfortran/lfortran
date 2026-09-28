! Names are case insensitive and blanks are allowed around "%", exactly
! as for component references.
module namespace_modules_22_Mixed
    implicit none
    integer :: Counter = 1
contains
    subroutine Reset()
        Counter = 0
    end subroutine
end module

program namespace_modules_22
    USE, NAMESPACE :: MX => NAMESPACE_MODULES_22_MIXED
    implicit none
    if (mx%counter /= 1) error stop
    if (MX%COUNTER /= 1) error stop
    Mx % Counter = 5
    if (mX%cOuNtEr /= 5) error stop
    call MX % reset()
    if (mx %counter /= 0) error stop
    print *, mx%counter
end program
