! A per-thread run-time sized temporary whose extent changes with the
! iteration: the result of a function that returns a copy of an
! allocatable component of the iteration's element of an array of derived
! type, assigned to a section or bound to an associate name, and local
! allocatable arrays sized from the loop indices. The host sizes each
! thread's slice of the workspace by the largest extent over the
! iterations, and each thread uses the part its own extent spans. The
! largest extent is in neither the first nor the last iteration.
module gpu_metal_388_m
    implicit none
    type :: tensor_t
        real, allocatable :: values_(:)
    contains
        procedure :: values
    end type
contains
    pure function values(self) result(v)
        class(tensor_t), intent(in) :: self
        real, allocatable :: v(:)
        v = self%values_
    end function
end module

program gpu_metal_388
    use gpu_metal_388_m
    implicit none
    type(tensor_t) :: inputs(4)
    integer :: m(4), k, j, i
    real :: a(6, 4), s(4), s2(4, 3), s3(4)
    m = [1, 5, 2, 3]
    do k = 1, 4
        allocate(inputs(k)%values_(m(k)))
        do i = 1, m(k)
            inputs(k)%values_(i) = real(i * k)
        end do
    end do

    a = -1.0
    do concurrent (k = 1:4)
        a(1:m(k), k) = inputs(k)%values()
    end do
    do k = 1, 4
        do i = 1, 6
            if (i <= m(k)) then
                if (abs(a(i, k) - real(i * k)) > 1.0e-6) error stop 1
            else
                if (abs(a(i, k) + 1.0) > 1.0e-6) error stop 2
            end if
        end do
    end do

    do concurrent (k = 1:4)
        associate (y => inputs(k)%values())
            s(k) = sum(y) + real(size(y))
        end associate
    end do
    do k = 1, 4
        if (abs(s(k) - real(k * m(k) * (m(k) + 1) / 2 + m(k))) > 1.0e-4) &
            error stop 3
    end do

    do concurrent (k = 1:4, j = 1:3)
        block
            real, allocatable :: w(:, :)
            integer :: c
            allocate(w(m(k), j))
            do c = 1, size(w, 2)
                w(:, c) = real(c)
            end do
            s2(k, j) = real(size(w, 1) * 10 + size(w, 2) * 100)
            do c = 1, size(w, 2)
                s2(k, j) = s2(k, j) + sum(w(:, c))
            end do
        end block
    end do
    do j = 1, 3
        do k = 1, 4
            if (abs(s2(k, j) - real(m(k) * j * (j + 1) / 2 + m(k) * 10 &
                    + j * 100)) > 1.0e-4) error stop 4
        end do
    end do

    do concurrent (k = 1:4)
        block
            integer :: n
            real, allocatable :: w(:)
            n = 2 * m(k) + 1
            allocate(w(n))
            w = real(k)
            s3(k) = sum(w)
        end block
    end do
    do k = 1, 4
        if (abs(s3(k) - real(k * (2 * m(k) + 1))) > 1.0e-4) error stop 5
    end do
    print *, "PASS"
end program
