! Namespace imports in the specification part of subroutines, functions,
! BLOCK constructs and module procedures; they are local to that scope.
module namespace_modules_12_a
    implicit none
    integer, parameter :: k = 10
contains
    integer function f(i)
        integer, intent(in) :: i
        f = i + k
    end function
end module

module namespace_modules_12_b
    implicit none
    integer, parameter :: k = 20
contains
    integer function f(i)
        integer, intent(in) :: i
        f = i + k
    end function
end module

module namespace_modules_12_user
    implicit none
contains
    integer function via_module_procedure(i)
        use, namespace :: a => namespace_modules_12_a
        integer, intent(in) :: i
        via_module_procedure = a%f(i)
    end function
end module

integer function external_fn(i)
    use, namespace :: b => namespace_modules_12_b
    implicit none
    integer, intent(in) :: i
    external_fn = b%f(i) + b%k
end function

program namespace_modules_12
    use namespace_modules_12_user, only: via_module_procedure
    implicit none
    interface
        integer function external_fn(i)
            integer, intent(in) :: i
        end function
    end interface
    integer :: a, b

    ! "a" and "b" are ordinary local variables here; the namespaces with
    ! these names only exist inside the scopes that declare them.
    a = 1
    b = 2
    call sub(a)
    if (a /= 11) error stop
    if (via_module_procedure(1) /= 11) error stop
    if (external_fn(1) /= 41) error stop
    block
        use, namespace :: nsb => namespace_modules_12_b
        b = nsb%f(b)
    end block
    if (b /= 22) error stop
    print *, a, b
contains
    subroutine sub(x)
        use, namespace :: a => namespace_modules_12_a
        integer, intent(inout) :: x
        x = a%f(x)
    end subroutine
end program
