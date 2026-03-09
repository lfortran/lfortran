program intrinsics_421
    implicit none
    integer :: cs
    character(200) :: cmsg

    cs = -1
    cmsg = ""
    call execute_command_line("echo hello > /dev/null 2>&1", cmdstat=cs, cmdmsg=cmsg)
    if (cs /= 0) error stop "cmdstat should be 0 for valid command"

    cs = 0
    cmsg = ""
    call execute_command_line("FOOBAR_NONEXISTENT_CMD 2>/dev/null", cmdstat=cs, cmdmsg=cmsg)
    if (cs <= 0) error stop "cmdstat should be positive for invalid command"
    if (len_trim(cmsg) == 0) error stop "cmdmsg should be non-empty for invalid command"

    cs = -1
    call execute_command_line("echo test > /dev/null 2>&1", cmdstat=cs)
    if (cs /= 0) error stop "cmdstat should be 0 for valid command (no cmdmsg)"

    cs = 0
    call execute_command_line("FOOBAR_ANOTHER_BAD 2>/dev/null", cmdstat=cs)
    if (cs <= 0) error stop "cmdstat should be positive for invalid command (no cmdmsg)"
end program intrinsics_421