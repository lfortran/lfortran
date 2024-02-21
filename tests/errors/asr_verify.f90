program main
    real(4) :: x(5) = [1, 2, 3, 4, 5]
    print*, sum(x, mask = .true.)
end