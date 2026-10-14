! A polymorphic intent(out) dummy is reset from its dynamic type even when
! the procedure was compiled before the extension existed, and the unit that
! passes the object never names the extension: the extension's own unit
! provides its reset. Its final subroutine runs first and sees the old
! values, then that of the parent type; the object is then default
! initialized and its allocatable component deallocated.
program intent_out_dynamic_type_01
    use intent_out_dynamic_type_01_base, only: Base, reset, base_finals
    use intent_out_dynamic_type_01_ext, only: make, is_reset, ext_finals, &
        seen_extra, seen_data
    implicit none
    class(Base), allocatable :: p
    call make(p, 42)
    call reset(p)
    if (ext_finals /= 1 .or. base_finals /= 1) error stop 1
    if (seen_extra /= 42 .or. .not. seen_data) error stop 2
    if (.not. is_reset(p)) error stop 3
    print '(a)', 'intent_out_dynamic_type_01 ok'
end program
