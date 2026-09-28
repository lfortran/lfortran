! Error: a module name is not a module entity (D2), also when a module entity
! for the same module was used before in the scope.
module namespace_modules_34_m
    implicit none
    type :: t
        integer :: i = 1
    end type
end module

program namespace_modules_34
    use, namespace :: x => namespace_modules_34_m
    implicit none
    type(x%t) :: a
    type(namespace_modules_34_m%t) :: b
    print *, a%i, b%i
end program
