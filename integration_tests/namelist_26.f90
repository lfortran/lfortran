program namelist_26
    implicit none

    character(len=15), allocatable :: name(:)
    character(len=:), allocatable :: cmd, cmd2
    character(len=256) :: test

    namelist /mylist/ cmd, name, cmd2

    cmd = repeat(' ', 132)
    cmd2 = repeat(' ', 132)
    allocate(name(2))

    test = '&MYLIST ' // &
           'CMD="run my_project", NAME="John Doe", CMD2="extra command" /'

    read(test, nml=mylist)

    print *, 'CMD = ', trim(cmd)
    print *, 'NAME(1) = ', name(1)
    print *, 'CMD2 = ', trim(cmd2)

    if (trim(cmd) /= 'run my_project') then
        error stop 'Incorrect CMD value read'
    end if
    if (trim(name(1)) /= 'John Doe') then
        error stop 'Incorrect NAME(1) value read'
    end if
    if (trim(cmd2) /= 'extra command') then
        error stop 'Incorrect CMD2 value read'
    end if
end program namelist_26