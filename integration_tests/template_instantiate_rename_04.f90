! The instantiation is loaded from the .mod file of its module.
program template_instantiate_rename_04
    use template_instantiate_rename_04_m2
    implicit none
    type(o) :: a
    call set(a, 3)
    if (a%v /= 3) error stop
    if (get(a) /= 3) error stop
    a = mk(5)
    if (a%v /= 5) error stop
    print *, "ok"
end program
