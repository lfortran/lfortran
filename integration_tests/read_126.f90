program read_126
    ! List-directed internal read of NaN(1) into real(selected_real_kind(33)).
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    real(qp) :: x
    integer :: ios
    character(len=8) :: buf
    character(len=16) :: shown

    x = 0.0_qp
    buf = 'NaN'
    read(buf, *, iostat=ios) x
    if (ios /= 0) error stop "plain nan iostat"
    write(shown, '(f16.1)') x
    if (adjustl(shown) /= 'NaN') error stop "plain nan"

    x = 0.0_qp
    buf = 'NaN(1)'
    read(buf, *, iostat=ios) x
    if (ios /= 0) error stop "payload iostat"
    write(shown, '(f16.1)') x
    if (adjustl(shown) /= 'NaN') error stop "payload nan"

    print *, "ok"
end program
