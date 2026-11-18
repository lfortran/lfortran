program class_intent_out_reset_02
    use class_intent_out_reset_02_base_m, only: base, reset
    use class_intent_out_reset_02_ext_m, only: child, probe
    implicit none
    type(child) :: object
    object%root = 7
    object%step = 9
    allocate(object%values(2), object%work(3))
    call reset(object)
    if (object%root /= -1 .or. object%step /= 2) error stop 1
    if (allocated(object%values) .or. allocated(object%work)) error stop 2
    if (probe(object) /= 210) error stop 3
    if (object%extra() /= 3) error stop 4
    print *, "ok"
end program
