module toml_types
    implicit none

    type :: toml_date
        integer :: y, m, d
    end type

    type :: toml_time
        integer :: hh = 0, mm = 0, ss = 0
    end type

    type :: toml_datetime
        type(toml_date) :: date
        type(toml_time) :: time
    end type

contains

    function make_date(y, m, d) result(res)
        integer, intent(in) :: y, m, d
        type(toml_date) :: res
        res%y = y
        res%m = m
        res%d = d
    end function

    function make_time() result(res)
        type(toml_time) :: res
    end function

    function make_datetime(d, t) result(res)
        type(toml_date), intent(in) :: d
        type(toml_time), intent(in) :: t
        type(toml_datetime) :: res
        res%date = d
        res%time = t
    end function

end module toml_types


module api
    use toml_types
    implicit none

contains

    subroutine set_value(arr, i, value, stat)
        type(toml_datetime), intent(inout) :: arr(:)
        integer, intent(in) :: i
        type(toml_datetime), intent(in) :: value
        integer, intent(out) :: stat

        arr(i) = value
        stat = 0
    end subroutine

end module api


program repro
    use toml_types
    use api
    implicit none

    type(toml_datetime) :: array(10)
    integer :: ii, stat

    ii = 3

    call set_value( &
        array, ii, &
        make_datetime( make_date(2022, ii, 7), make_time() ), &
        stat=stat &
    )

end program repro
