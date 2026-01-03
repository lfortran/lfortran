subroutine sub()
    character(len=:), allocatable :: buffer
    character(:), allocatable :: dummy

    allocate(character((5)) :: dummy)
    dummy = "12345"

    call move_alloc( dummy, buffer )

    print *, allocated(buffer)
    print *, buffer

    if (allocated(buffer) .neqv. .true.) error stop
    if (buffer /= "12345") error stop
end subroutine

program intrinsics_375
    call sub()
end program