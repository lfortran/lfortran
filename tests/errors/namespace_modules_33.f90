! Error: a module entity declared "use, namespace, private" is not
! accessible to users of the module, neither by ONLY nor by a plain USE.
module namespace_modules_33_a
    implicit none
    integer :: x = 1
end module

module namespace_modules_33_b
    use, namespace, private :: a => namespace_modules_33_a
    implicit none
    integer :: y = 2
end module

program namespace_modules_33
    use namespace_modules_33_b, only: a
    implicit none
    print *, a%x
end program
