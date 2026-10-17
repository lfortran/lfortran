program character_module_array_15364
    use character_module_array_15364_m
    implicit none

    if (initialized(1) /= 'ab ' .or. initialized(2) /= 'cde') &
        error stop 'initial values not visible'
    call set_arrays()
    if (w(1) /= 'p' .or. w(2) /= 'q') error stop 'w not shared'
    if (initialized(1) /= 'xy ' .or. initialized(2) /= 'uvw') &
        error stop 'initialized not shared'

    w = ['r', 's']
    initialized = ['one', 'two']
    call check_arrays()
end program
