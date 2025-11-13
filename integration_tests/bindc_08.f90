program optional_06
    use iso_c_binding, only: c_char, c_int, c_null_char
    implicit none

    interface
        function c_is_dir(path) result(r) bind(c, name="c_is_dir")
            import c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(kind=c_int) :: r
        end function c_is_dir
    end interface
    character(:), allocatable :: tmp_chars(:)
    integer :: res
    tmp_chars = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J']
    ! res = c_is_dir(tmp_chars)  !! TODO: Handle descriptor array passed to bind c
end program optional_06