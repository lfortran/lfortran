! DECISION D4 (export of namespaces through USE association).
! Valid under options A and A2; an error under options B and C.
!
! A module with default PRIVATE accessibility explicitly publishes a
! namespace. Users can import it with ONLY, rename it, or reach it
! through a namespace of the module.
module namespace_modules_export_03_a
    implicit none
    integer :: x = 1
contains
    integer function get_x()
        get_x = x
    end function
end module

module namespace_modules_export_03_b
    use, namespace :: a => namespace_modules_export_03_a
    implicit none
    private
    public :: a, y
    integer :: y = 2
end module

program namespace_modules_export_03
    use namespace_modules_export_03_b, only: a
    use namespace_modules_export_03_b, only: other => a
    use, namespace :: b => namespace_modules_export_03_b
    implicit none
    if (a%x /= 1) error stop
    other%x = 5
    if (a%get_x() /= 5) error stop
    if (b%a%x /= 5) error stop
    if (b%y /= 2) error stop
    print *, a%x, other%x, b%a%x
end program
