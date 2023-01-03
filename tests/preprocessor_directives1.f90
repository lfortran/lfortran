module preprocessor_directives1
    implicit none
    private

#ifndef _WIN32
    character(len=*), parameter :: pwd_env = "PWD"
#else
    character(len=*), parameter :: pwd_env = "CD"
#endif

    interface
        function chdir(path) result(stat) &
#ifndef _WIN32
                bind(C, name="chdir")
#else
                bind(C, name="_chdir")
#endif
            import :: c_char, c_int
            character(kind=c_char, len=1), intent(in) :: path(*)
            integer(c_int) :: stat
        end function chdir
    end interface

end module preprocessor_directives1
