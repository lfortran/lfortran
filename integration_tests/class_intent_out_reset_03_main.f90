program class_intent_out_reset_03_main
    use class_intent_out_reset_03_types_m, only: child, holder
    use class_intent_out_reset_03_copy_m, only: copy_objects, copy_holder
    use class_intent_out_reset_03_reset_m, only: reset
    implicit none
    type(child) :: a(1), b(1)
    type(holder) :: x, y
    allocate(y%elements(1))
    call copy_holder(x, y)
    if (.not. allocated(x%elements)) error stop 1
    b(1)%root = 7
    b(1)%step = 9
    allocate(b(1)%work(3))
    call copy_objects(a, b)
    call reset(a(1))
    if (a(1)%root /= -1 .or. a(1)%step /= 2) error stop 2
    if (allocated(a(1)%work)) error stop 3
    if (b(1)%step /= 9 .or. .not. allocated(b(1)%work)) error stop 4
    print *, "ok"
end program
