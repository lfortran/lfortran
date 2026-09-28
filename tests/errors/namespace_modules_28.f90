! Error: a namespace that a module declares PRIVATE is not accessible to
! users of that module, neither by an ordinary USE nor through a
! namespace of the module (under every option of decision D4).
module namespace_modules_28_a
    implicit none
    integer :: x = 1
end module

module namespace_modules_28_b
    use, namespace :: a => namespace_modules_28_a
    implicit none
    private :: a
end module

program namespace_modules_28
    use, namespace :: b => namespace_modules_28_b
    implicit none
    print *, b%a%x
end program
