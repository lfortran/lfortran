! The only user of global_init_19_m.
subroutine global_init_19_s()
    use global_init_19_m
    implicit none
    if (.not. associated(pt, tgt)) error stop 1
    if (size(pt) /= 4 .or. sum(pt) /= 10) error stop 2
    ! associated(psec, tgt(2:3)) is avoided: it is wrongly .false. for an
    ! array section target even for a pointer assigned at run time
    ! (unrelated). The extent, the values and a write through the pointer
    ! check the association instead.
    if (.not. associated(psec)) error stop 3
    if (size(psec) /= 2 .or. sum(psec) /= 5) error stop 4
    pt(4) = 40
    psec(1) = 20
    if (tgt(4) /= 40 .or. tgt(2) /= 20) error stop 5
    print *, "ok"
end subroutine global_init_19_s
