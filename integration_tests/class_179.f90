program class_179
    use class_179_base_m, only: reset
    use class_179_child_m, only: child, pointer_defaults_restored, add_three
    implicit none
    type(child) :: x
    integer, target :: target = 11
    if (.not. pointer_defaults_restored(x)) error stop 6
    x%root = 9
    x%part(1)%mark = 9
    x%part(2)%mark = 17
    allocate(x%part(1)%values(2))
    x%part(1)%ref => target
    x%chosen => target
    x%part(1)%restored => target
    x%part(2)%restored => target
    x%from_array => target
    x%from_member => target
    x%callback => add_three
    call reset(x)
    if (x%root /= -1) error stop 1
    if (any(x%part%mark /= 42)) error stop 2
    if (allocated(x%part(1)%values)) error stop 3
    if (associated(x%part(1)%ref)) error stop 4
    if (target /= 11) error stop 5
    if (.not. pointer_defaults_restored(x)) error stop 7
    if (x%chosen /= 17 .or. x%part(2)%restored /= 17) error stop 8
    if (x%callback(5) /= 7) error stop 9
    if (x%from_array /= 2 .or. x%from_member /= 33) error stop 10
end program
