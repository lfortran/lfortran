program namelist_36
    implicit none

    type :: options
        integer :: maxcalls = 0
        real(8) :: mintime = 0.0d0
        character(20) :: name = ''
    end type

    integer, target :: count_t
    integer, pointer :: count => null()
    real(8), allocatable :: tol
    type(options), target :: opts_t
    type(options), pointer :: opts => null()

    namelist /config/ count, tol, opts

    count => count_t
    allocate(tol)
    opts => opts_t

    count = 42
    tol = 0.125d0
    opts%maxcalls = 10
    opts%mintime = 50.0d0
    opts%name = 'run1'

    open(10, status="scratch", delim="apostrophe")
    write(10, nml=config)
    rewind(10)

    count = 0
    tol = 0.0d0
    opts%maxcalls = 0
    opts%mintime = 0.0d0
    opts%name = ''
    read(10, nml=config)
    close(10)

    if (count /= 42) error stop
    if (tol /= 0.125d0) error stop
    if (opts%maxcalls /= 10) error stop
    if (opts%mintime /= 50.0d0) error stop
    if (trim(opts%name) /= 'run1') error stop
    print *, "scalar pointer/allocatable namelist ok"
end program namelist_36
