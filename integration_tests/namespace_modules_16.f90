! Member names may coincide with intrinsic procedure names, with the
! namespace's own local name, and with names of other namespaces; the
! qualified reference is never ambiguous.
module namespace_modules_16_m
    implicit none
    integer :: v = 5
    integer :: m = 6
contains
    integer function size(a)
        integer, intent(in) :: a(:)
        size = 1000 + a(1)
    end function

    integer function max(a, b)
        integer, intent(in) :: a, b
        max = a + b
    end function
end module

program namespace_modules_16
    ! The namespace is named "v", like one of its members
    use, namespace :: v => namespace_modules_16_m
    ! A second namespace for the same module, named "m", like another member
    use, namespace :: m => namespace_modules_16_m
    implicit none
    integer :: a(3) = [1, 2, 3]

    if (v%v /= 5) error stop
    if (v%m /= 6) error stop
    if (m%v /= 5) error stop
    if (m%m /= 6) error stop
    ! The module's size and max do not hide the intrinsics
    if (size(a) /= 3) error stop
    if (max(2, 7) /= 7) error stop
    if (v%size(a) /= 1001) error stop
    if (m%max(2, 7) /= 9) error stop
    print *, v%v, m%m, v%size(a), max(2, 7)
end program
