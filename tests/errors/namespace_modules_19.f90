! Error: a DO variable must be a variable name, not a namespace-qualified
! name.
module namespace_modules_19_m
    implicit none
    integer :: i = 0
end module

program namespace_modules_19
    use, namespace :: m => namespace_modules_19_m
    implicit none
    do m%i = 1, 3
        print *, m%i
    end do
end program
