! DECISION D4 (export of namespaces through USE association).
! Valid under options A and B; an error under options A2 and C.
!
! A "facade" module collects other modules as namespaces. Users reach the
! members through a chain of namespaces: std%linalg%solve2.
module namespace_modules_export_01_linalg
    implicit none
contains
    ! Solve the diagonal system diag(d) x = b
    function solve2(d, b) result(x)
        real, intent(in) :: d(2), b(2)
        real :: x(2)
        x = b / d
    end function
end module

module namespace_modules_export_01_stats
    implicit none
contains
    real function mean(x)
        real, intent(in) :: x(:)
        mean = sum(x) / size(x)
    end function
end module

module namespace_modules_export_01_std
    use, namespace :: linalg => namespace_modules_export_01_linalg
    use, namespace :: stats => namespace_modules_export_01_stats
    implicit none
    character(len=*), parameter :: version = "1.0"
end module

program namespace_modules_export_01
    use, namespace :: std => namespace_modules_export_01_std
    implicit none
    real :: x(2)

    x = std%linalg%solve2([2.0, 4.0], [2.0, 2.0])
    if (any(abs(x - [1.0, 0.5]) > 1e-6)) error stop
    if (abs(std%stats%mean([1.0, 2.0, 6.0]) - 3.0) > 1e-6) error stop
    if (std%version /= "1.0") error stop
    print *, x, std%stats%mean(x)
end program
