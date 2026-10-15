! The instantiations inside module procedures are loaded from the .mod file of
! their module.
program template_instantiate_rename_05
    use template_instantiate_rename_05_m2
    implicit none
    if (run(3) /= 4) error stop
    if (abs(run_internal(1.5) - 1.5) > 1e-6) error stop
    print *, "ok"
end program
