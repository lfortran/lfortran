program reachable_proc_01
    use reachable_proc_01_mod, only: f1
    implicit none
    if (f1(1) /= 17) error stop 1
    print *, "ok"
end program
