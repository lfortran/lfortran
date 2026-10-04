! Under --detect-leaks, the object file that defines a module frees what the
! module's storage owns before the leak report counts, through a teardown it
! registers with the runtime. `module_teardown_02_b` owns `a`, which is still
! allocated when the program ends, so its teardown is what keeps the report
! clean. `module_teardown_02_a` owns nothing, and registers no teardown at
! all; the reference LLVM output of this file shows both.
module module_teardown_02_a
    implicit none
    integer :: x = 1
end module module_teardown_02_a

module module_teardown_02_b
    implicit none
    integer, allocatable :: a(:)
end module module_teardown_02_b

program module_teardown_02
    use module_teardown_02_a, only: x
    use module_teardown_02_b, only: a
    implicit none
    allocate(a(x))
    if (size(a) /= 1) error stop 1
    print *, "ok"
end program module_teardown_02
