! The update of a layered model's weights in a `do concurrent` over the
! layers: the statements over the same section of a layer run in one loop
! over its elements, which the lanes of a SIMD-group share
! (gpu_vector_lanes.cpp). With more iterations than a launch gives lanes
! to, each iteration runs on one thread; both must give the result of the
! statements run one after another.
module gpu_metal_472_m
implicit none
contains
    subroutine adam(n, nl, dcdw, vdw, sdw, w, dcdb, vdb, b, mb, alpha)
        integer, intent(in) :: n(0:), nl, mb
        real, intent(inout) :: dcdw(:,:,:), vdw(:,:,:), sdw(:,:,:), w(:,:,:)
        real, intent(inout) :: dcdb(:,:), vdb(:,:), b(:,:)
        real, intent(in) :: alpha
        integer :: l
        do concurrent (l = 1:nl)
            dcdw(1:n(l),1:n(l-1),l) = dcdw(1:n(l),1:n(l-1),l) / mb
            vdw(1:n(l),1:n(l-1),l) = 0.9*vdw(1:n(l),1:n(l-1),l) &
                + 0.1*dcdw(1:n(l),1:n(l-1),l)
            sdw(1:n(l),1:n(l-1),l) = 0.99*sdw(1:n(l),1:n(l-1),l) &
                + 0.01*dcdw(1:n(l),1:n(l-1),l)**2
            w(1:n(l),1:n(l-1),l) = w(1:n(l),1:n(l-1),l) &
                - alpha*vdw(1:n(l),1:n(l-1),l)/(sqrt(sdw(1:n(l),1:n(l-1),l)) + 1e-8)
            dcdb(1:n(l),l) = dcdb(1:n(l),l) / mb
            vdb(1:n(l),l) = 0.9*vdb(1:n(l),l) + 0.1*dcdb(1:n(l),l)
            b(1:n(l),l) = b(1:n(l),l) - alpha*vdb(1:n(l),l)
        end do
    end subroutine
end module

program gpu_metal_472
use gpu_metal_472_m
implicit none
integer :: nl, i, j, l
integer, allocatable :: n(:)
real, allocatable :: dcdw(:,:,:), vdw(:,:,:), sdw(:,:,:), w(:,:,:)
real, allocatable :: dcdb(:,:), vdb(:,:), b(:,:)
real, allocatable :: dcdw0(:,:,:), vdw0(:,:,:), sdw0(:,:,:), w0(:,:,:)
real, allocatable :: dcdb0(:,:), vdb0(:,:), b0(:,:)
! Few layers, and more layers than iterations that get lanes.
do nl = 3, 1503, 1500
    allocate(n(0:nl))
    do l = 0, nl
        n(l) = 1 + mod(l * 7, 5)
    end do
    allocate(dcdw(5,5,nl), vdw(5,5,nl), sdw(5,5,nl), w(5,5,nl), &
        dcdb(5,nl), vdb(5,nl), b(5,nl))
    do l = 1, nl
        do j = 1, 5
            dcdb(j, l) = 0.1 * j + 0.01 * l
            vdb(j, l) = 0.2
            b(j, l) = 1
            do i = 1, 5
                dcdw(i, j, l) = 0.1 * i - 0.05 * j + 0.001 * l
                vdw(i, j, l) = 0.5
                sdw(i, j, l) = 1
                w(i, j, l) = 1
            end do
        end do
    end do
    dcdw0 = dcdw; vdw0 = vdw; sdw0 = sdw; w0 = w; dcdb0 = dcdb; vdb0 = vdb; b0 = b
    call adam(n, nl, dcdw, vdw, sdw, w, dcdb, vdb, b, 2, 0.01)
    do l = 1, nl
        dcdw0(1:n(l),1:n(l-1),l) = dcdw0(1:n(l),1:n(l-1),l) / 2
        vdw0(1:n(l),1:n(l-1),l) = 0.9*vdw0(1:n(l),1:n(l-1),l) &
            + 0.1*dcdw0(1:n(l),1:n(l-1),l)
        sdw0(1:n(l),1:n(l-1),l) = 0.99*sdw0(1:n(l),1:n(l-1),l) &
            + 0.01*dcdw0(1:n(l),1:n(l-1),l)**2
        w0(1:n(l),1:n(l-1),l) = w0(1:n(l),1:n(l-1),l) &
            - 0.01*vdw0(1:n(l),1:n(l-1),l)/(sqrt(sdw0(1:n(l),1:n(l-1),l)) + 1e-8)
        dcdb0(1:n(l),l) = dcdb0(1:n(l),l) / 2
        vdb0(1:n(l),l) = 0.9*vdb0(1:n(l),l) + 0.1*dcdb0(1:n(l),l)
        b0(1:n(l),l) = b0(1:n(l),l) - 0.01*vdb0(1:n(l),l)
    end do
    if (maxval(abs(w - w0)) > 1e-5 .or. maxval(abs(vdw - vdw0)) > 1e-5 &
        .or. maxval(abs(sdw - sdw0)) > 1e-5 .or. maxval(abs(dcdw - dcdw0)) > 1e-5) &
        error stop "weights"
    if (maxval(abs(b - b0)) > 1e-5 .or. maxval(abs(vdb - vdb0)) > 1e-5 &
        .or. maxval(abs(dcdb - dcdb0)) > 1e-5) error stop "biases"
    deallocate(n, dcdw, vdw, sdw, w, dcdb, vdb, b, dcdw0, vdw0, sdw0, w0, &
        dcdb0, vdb0, b0)
end do
print *, "ok"
end program
