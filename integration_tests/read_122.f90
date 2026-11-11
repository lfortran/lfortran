program read_122
    ! List-directed internal read of NaN and Inf into real(selected_real_kind(33)).
    implicit none
    integer, parameter :: qp = selected_real_kind(33)
    real(qp) :: x
    character(len=16) :: buf

    call check('NaN', 'NaN')
    call check('Inf', 'Infinity')
    call check('-Inf', '-Infinity')
    call check('Infinity', 'Infinity')
    call check('-Infinity', '-Infinity')

    buf = '1.25'
    read(buf, *) x
    if (x /= 1.25_qp) error stop "finite"

    print *, "ok"
contains
    subroutine check(text, expect)
        character(len=*), intent(in) :: text, expect
        character(len=32) :: src
        src = text
        read(src, *) x
        write(buf, '(f16.1)') x
        if (adjustl(buf) /= expect) error stop text
    end subroutine check
end program read_122
