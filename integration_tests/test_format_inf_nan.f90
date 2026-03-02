program test_format_inf_nan
    implicit none
    character(12) :: cnaninfs = 'NAN INF -INF'
    real :: naninfs(3)
    character(15) :: buf

    read(cnaninfs, *) naninfs

    write(buf, "(3F5.0)") naninfs
    if (buf /= "  NaN  Inf -Inf") error stop

    write(buf, "(3F5.0)") naninfs(1), naninfs(2), naninfs(3)
    if (buf /= "  NaN  Inf -Inf") error stop

    print "(3F5.0)", naninfs
end program test_format_inf_nan
