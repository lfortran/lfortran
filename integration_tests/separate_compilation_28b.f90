subroutine caller_one()
    character(32) :: cvar
    common /cblk/ cvar
    cvar = 'CALLER_ONE'
    call string_handler('CALLER_ONE', 1)
end subroutine

subroutine caller_two()
    character(32) :: cvar
    common /cblk/ cvar
    cvar = 'CALLER_TWO'
    call string_handler('CALLER_TWO', 2)
end subroutine
