program associate_50
implicit none
real :: pi_val, d_val, g_val
logical :: cmp_result

! Test nested associate where one associate variable references another
associate(pi => 4.0*atan(1.0))
    associate(d => sin(pi/6.0))
        associate(g => d)
            pi_val = pi
            d_val = d
            g_val = g
            cmp_result = (g == d)

            if (abs(pi_val - 3.14159265) > 1e-5) error stop
            if (abs(d_val - 0.5) > 1e-5) error stop
            if (abs(g_val - 0.5) > 1e-5) error stop
            if (.not. cmp_result) error stop
            if (abs(g_val - d_val) > 1e-7) error stop

            ! Test arithmetic with nested associates
            if (abs(g + d - 1.0) > 1e-5) error stop
            if (abs(g * d - 0.25) > 1e-5) error stop
        end associate
    end associate
end associate

print *, "All tests passed"
end program
