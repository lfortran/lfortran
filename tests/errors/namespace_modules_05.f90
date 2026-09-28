! Error: private module entities are not accessible through a namespace.
module namespace_modules_05_m
    implicit none
    private
    public :: visible
    integer :: visible = 1
    integer :: hidden = 2
end module

program namespace_modules_05
    use, namespace :: m => namespace_modules_05_m
    implicit none
    print *, m%visible, m%hidden
end program
