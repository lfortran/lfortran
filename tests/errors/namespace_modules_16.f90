! Error: the double colon is required after the NAMESPACE modifier, as for
! the INTRINSIC and NON_INTRINSIC modifiers.
module namespace_modules_16_m
    implicit none
    integer :: x = 1
end module

program namespace_modules_16
    use, namespace m => namespace_modules_16_m
    implicit none
    print *, m%x
end program
