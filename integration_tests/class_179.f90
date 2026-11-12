program class_179
    use class_179_base_m, only: reset
    use class_179_child_m, only: child
    implicit none
    type(child) :: x
    integer, target :: target = 11
    x%root = 9
    x%part(1)%mark = 9
    x%part(2)%mark = 17
    allocate(x%part(1)%values(2))
    x%part(1)%ref => target
    call reset(x)
    if (x%root /= -1) error stop 1
    if (any(x%part%mark /= 42)) error stop 2
    if (allocated(x%part(1)%values)) error stop 3
    if (associated(x%part(1)%ref)) error stop 4
    if (target /= 11) error stop 5
end program
