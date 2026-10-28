module string_mod
    public
contains
    !> Fixed-length string
    function string()
       character(len=6) :: string
       string = "abcdef"
    end function
end module

program compiler_version_03
    use string_mod, only: string

! semantic error: The symbol 'compiler_version' not found in the module 'iso_fortran_env'
    use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
    
    character(len=len(compiler_version())) :: a
    character(len=len(string())) :: b
    character(len=:), allocatable :: c
    character(len=len(compiler_options())) :: options
    character(len=1+len(compiler_version())) :: decorated

    a = compiler_version()
    print *, len(a), a
    
    b = string()
    print *, len(b), b
    if ( len(b) /= 6 ) error stop
    
    c = compiler_version()
    print *, len(c), c

    if(len(c) /= len(a)) error stop
    if(c /= a) error stop
    options = compiler_options()
    if (len(options) /= len(compiler_options())) error stop
    if (options /= compiler_options()) error stop
    decorated = compiler_version() // '!'
    if (len(decorated) /= len(a)+1) error stop
    if (decorated /= a // '!') error stop
end program
