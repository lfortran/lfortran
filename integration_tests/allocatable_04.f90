program allocatable_04
    implicit none
    call check(.true.)
    call check(.false.)
contains
    subroutine check(first)
        logical, intent(in) :: first
        character(:), allocatable, save :: s
        character(:), allocatable :: automatic
        if (allocated(automatic)) error stop 1
        if (first) then
            if (allocated(s)) error stop 2
            s = "saved"
        else
            if (.not. allocated(s)) error stop 3
            if (len(s) /= 5) error stop 4
            if (s /= "saved") error stop 5
            deallocate(s)
        end if
        automatic = "local"
    end subroutine
end program
