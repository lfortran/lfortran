module external_26_mod
    implicit none
    ! The procedures are defined in external_26_procs.f90, which has no
    ! main program.
    interface
        subroutine external_26_add(x, y, z)
            integer, intent(in) :: x, y
            integer, intent(out) :: z
        end subroutine external_26_add

        integer function external_26_twice(x)
            integer, intent(in) :: x
        end function external_26_twice
    end interface
end module external_26_mod

program external_26
    use external_26_mod, only: external_26_add, external_26_twice
    implicit none
    integer :: z
    call external_26_add(3, 4, z)
    if (z /= 7) error stop
    if (external_26_twice(z) /= 14) error stop
    print *, z, external_26_twice(z)
end program external_26
