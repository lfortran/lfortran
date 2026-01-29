program optional_08
    implicit none

    type :: base
        character(len=:), allocatable :: working_dir
    end type base

    class(base), allocatable :: obj
    character(len=:), allocatable :: working_dir

    ! ---------------------------
    ! Initial setup
    ! ---------------------------
    working_dir = "Hello"

    if (.not. allocated(working_dir)) then
        error stop "ERROR: working_dir not allocated after assignment"
    end if

    ! ---------------------------
    ! Get command-line settings
    ! ---------------------------
    call get_command_line_settings(obj)

    if (.not. allocated(obj)) then
        error stop "ERROR: obj not allocated by get_command_line_settings"
    end if

    ! ---------------------------
    ! Extract working directory
    ! ---------------------------
    call get_working_dir(obj, working_dir)

    if (.not. allocated(working_dir)) then
        error stop "ERROR: working_dir not allocated after get_working_dir"
    end if

    print *, "SUCCESS"
    print *, "working_dir =", trim(working_dir)

contains

    subroutine get_working_dir(settings, working_dir_)
        class(base), optional, intent(in) :: settings
        character(len=:), allocatable, intent(out) :: working_dir_

        ! Validate presence
        if (.not. present(settings)) then
            error stop "ERROR: settings not present in get_working_dir"
        end if

        ! Validate allocation of component
        if (.not. allocated(settings%working_dir)) then
            error stop "ERROR: settings%working_dir not allocated"
        end if

        ! Safe assignment (auto-allocates working_dir_)
        working_dir_ = settings%working_dir
    end subroutine get_working_dir

    subroutine get_command_line_settings(cmd_settings)
        class(base), allocatable, intent(out) :: cmd_settings

        allocate(cmd_settings)

        if (.not. allocated(cmd_settings)) then
            error stop "ERROR: failed to allocate cmd_settings"
        end if

        ! Initialize component explicitly
        cmd_settings%working_dir = "Hello"

        if (.not. allocated(cmd_settings%working_dir)) then
            error stop "ERROR: cmd_settings%working_dir not allocated"
        end if
    end subroutine get_command_line_settings

end program
