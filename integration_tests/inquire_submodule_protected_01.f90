! A submodule accesses the PROTECTED variables of its parent module by host
! association, so INQUIRE can define them there.
module inquire_submodule_protected_01_m
    implicit none
    logical, protected :: is_open = .false.
    integer, protected :: unit_number = 0
    character(len=20), protected :: delim_mode = ""
    character(len=20), protected :: action_mode = ""
    integer, protected :: ios = -1
    interface
        module subroutine inquire_unit(u)
            integer, intent(in) :: u
        end subroutine
    end interface
end module

submodule (inquire_submodule_protected_01_m) inquire_submodule_protected_01_s
contains
    module subroutine inquire_unit(u)
        integer, intent(in) :: u
        inquire(unit=u, opened=is_open, number=unit_number, &
            delim=delim_mode, action=action_mode, iostat=ios)
    end subroutine
end submodule

program inquire_submodule_protected_01
    use inquire_submodule_protected_01_m
    implicit none
    integer :: u
    open(newunit=u, status="scratch", form="formatted", delim="quote", &
        action="readwrite")
    call inquire_unit(u)
    close(u)
    print *, is_open, unit_number, trim(delim_mode), " ", trim(action_mode), ios
    if (.not. is_open) error stop 1
    if (unit_number /= u) error stop 2
    if (delim_mode /= "QUOTE") error stop 3
    if (action_mode /= "READWRITE") error stop 4
    if (ios /= 0) error stop 5
end program
