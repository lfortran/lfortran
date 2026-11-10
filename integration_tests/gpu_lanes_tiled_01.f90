program gpu_lanes_tiled_01
! A nest of 48 x 128 iterations is launched in tiles on CUDA and each of its
! iterations runs on the lanes of a warp (gpu_vector_lanes.cpp): the lanes
! need the iterations numbered flat, or only 1/32 of them run.
    implicit none
    integer, parameter :: hv = 48, dv = 128
    real :: state(dv, dv, hv), g(hv), v(dv, hv), out(dv, dv, hv), tot(dv, hv)
    integer :: h, row, k, bad
    real :: s
    do h = 1, hv
        g(h) = 0.5 + 0.01*h
        do k = 1, dv
            v(k, h) = 0.001*k + 0.01*h
        end do
    end do
    do h = 1, hv
        do row = 1, dv
            do k = 1, dv
                state(k, row, h) = 0.001*k + 0.002*row + 0.0001*h
            end do
        end do
    end do
    do concurrent (h = 1:hv, row = 1:dv)
        block
            state(:, row, h) = state(:, row, h)*g(h)
            tot(row, h) = sum(state(:, row, h)*v(:, h))
            out(:, row, h) = state(:, row, h) + tot(row, h)
        end block
    end do
    bad = 0
    do h = 1, hv
        do row = 1, dv
            s = 0
            do k = 1, dv
                s = s + (0.001*k + 0.002*row + 0.0001*h)*g(h)*v(k, h)
            end do
            if (abs(tot(row, h) - s) > 1e-3*abs(s)) bad = bad + 1
        end do
    end do
    print *, "wrong rows:", bad, "of", hv*dv
    if (bad /= 0) error stop 1
end program gpu_lanes_tiled_01
