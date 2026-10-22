! A file that uses a module compiled separately neither offloads nor reports
! the loops of the module's procedures: they are compiled with the module.
program gpu_separate_compilation_01
    use gpu_separate_compilation_01_mod, only: scale, report_negative, tri
    implicit none
    real :: x(4), y(5)
    integer :: j

    x = [1.0, 2.0, 3.0, 4.0]
    call scale(x)
    call report_negative(x)
    if (any(x /= [3.0, 6.0, 9.0, 12.0])) error stop 1

    do concurrent (j = 1:5)
        y(j) = tri(j)
    end do
    do j = 1, 5
        if (y(j) /= 36.0 * j) error stop 2
    end do
    print *, "ok"
end program
