! The character components of module variables get storage of their declared
! length even when the program does not use the module, so that compiled
! with --separate-compilation the module's object file is the only one that
! sets them up (#16066).
program separate_compilation_72
implicit none
external :: drv
call drv()
print *, 'ok'
end program
