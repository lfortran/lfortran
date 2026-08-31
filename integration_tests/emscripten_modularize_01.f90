program emscripten_modularize_01
    integer :: i, s
    s = 0
    do i = 1, 10
        s = s + i
    end do
    if (s /= 55) error stop
    print *, "PASS"
end program
