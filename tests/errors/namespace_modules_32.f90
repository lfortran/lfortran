! Error: at most one access-spec may appear in a namespace import.
module namespace_modules_32_a
    implicit none
    integer :: x = 1
end module

module namespace_modules_32_b
    use, namespace, public, private :: a => namespace_modules_32_a
    implicit none
end module

program namespace_modules_32
    implicit none
end program
