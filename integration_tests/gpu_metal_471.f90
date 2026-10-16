! A training step of a small neural network in a `do concurrent` with few
! iterations, as FIATS writes it: the array statements of an iteration --
! matmul, an elemental activation, the back-propagation, the gradient
! sums -- run on the lanes of a SIMD-group (gpu_vector_lanes.cpp), and a
! type-bound function returning an array runs on the first lane. The
! results must match the same step computed one statement at a time.
module gpu_metal_471_m
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
    elemental real function sigmoid(x)
        real, intent(in) :: x
        sigmoid = 1. / (1. + exp(-x))
    end function
    elemental real function sigmoid_prime(x)
        real, intent(in) :: x
        sigmoid_prime = sigmoid(x) * (1. - sigmoid(x))
    end function

    subroutine step(n, w, b, inputs, outputs, dcdw, dcdb, m)
        integer, intent(in) :: n(0:), m
        real, intent(in) :: w(:,:,:), b(:,:)
        type(tensor_t), intent(in) :: inputs(:), outputs(:)
        real, intent(out) :: dcdw(:,:,:,:), dcdb(:,:,:)
        integer :: pair, l, j, nl
        nl = ubound(n, 1)
        dcdw = 0.
        dcdb = 0.
        do concurrent (pair = 1:m)
            block
                real :: a(maxval(n), 0:nl), z(size(b,1), size(b,2)), &
                    delta(size(b,1), size(b,2))
                a(1:n(0), 0) = inputs(pair)%values()
                do l = 1, nl
                    z(1:n(l), l) = matmul(w(1:n(l), 1:n(l-1), l), &
                        a(1:n(l-1), l-1)) + b(1:n(l), l)
                    a(1:n(l), l) = sigmoid(z(1:n(l), l))
                end do
                associate (y => outputs(pair)%values())
                    delta(1:n(nl), nl) = (a(1:n(nl), nl) - y(1:n(nl))) &
                        * sigmoid_prime(z(1:n(nl), nl))
                end associate
                do l = nl - 1, 1, -1
                    delta(1:n(l), l) = matmul(transpose(w(1:n(l+1), 1:n(l), &
                        l+1)), delta(1:n(l+1), l+1)) * sigmoid_prime(z(1:n(l), l))
                end do
                do l = 1, nl
                    dcdb(1:n(l), l, pair) = dcdb(1:n(l), l, pair) + delta(1:n(l), l)
                    do j = 1, n(l)
                        dcdw(j, 1:n(l-1), l, pair) = dcdw(j, 1:n(l-1), l, pair) &
                            + a(1:n(l-1), l-1) * delta(j, l)
                    end do
                end do
            end block
        end do
    end subroutine

    ! The same step, one pair after another, outside any parallel loop.
    subroutine step_serial(n, w, b, inputs, outputs, dcdw, dcdb, m)
        integer, intent(in) :: n(0:), m
        real, intent(in) :: w(:,:,:), b(:,:)
        type(tensor_t), intent(in) :: inputs(:), outputs(:)
        real, intent(out) :: dcdw(:,:,:,:), dcdb(:,:,:)
        integer :: pair, l, j, k, nl
        real :: a(maxval(n), 0:ubound(n,1)), z(size(b,1), size(b,2)), &
            delta(size(b,1), size(b,2)), s
        nl = ubound(n, 1)
        dcdw = 0.
        dcdb = 0.
        do pair = 1, m
            a(1:n(0), 0) = inputs(pair)%values_
            do l = 1, nl
                do j = 1, n(l)
                    s = 0.
                    do k = 1, n(l-1)
                        s = s + w(j, k, l) * a(k, l-1)
                    end do
                    z(j, l) = s + b(j, l)
                    a(j, l) = sigmoid(z(j, l))
                end do
            end do
            do j = 1, n(nl)
                delta(j, nl) = (a(j, nl) - outputs(pair)%values_(j)) &
                    * sigmoid_prime(z(j, nl))
            end do
            do l = nl - 1, 1, -1
                do j = 1, n(l)
                    s = 0.
                    do k = 1, n(l+1)
                        s = s + w(k, j, l+1) * delta(k, l+1)
                    end do
                    delta(j, l) = s * sigmoid_prime(z(j, l))
                end do
            end do
            do l = 1, nl
                do j = 1, n(l)
                    dcdb(j, l, pair) = dcdb(j, l, pair) + delta(j, l)
                    do k = 1, n(l-1)
                        dcdw(j, k, l, pair) = dcdw(j, k, l, pair) + a(k, l-1) * delta(j, l)
                    end do
                end do
            end do
        end do
    end subroutine
end module

program gpu_metal_471
use gpu_metal_471_m
implicit none
integer, parameter :: nl = 3
integer :: n(0:nl) = [2, 3, 3, 1], m, i, j, l
real :: w(3, 3, nl), b(3, nl)
type(tensor_t), allocatable :: inputs(:), outputs(:)
real, allocatable :: dcdw(:,:,:,:), dcdb(:,:,:), dcdw_ref(:,:,:,:), dcdb_ref(:,:,:)
do l = 1, nl
    do j = 1, 3
        b(j, l) = 0.1 * j - 0.05 * l
        do i = 1, 3
            w(i, j, l) = 0.3 * i - 0.2 * j + 0.1 * l
        end do
    end do
end do
! One pair, as in a mini-batch of one, and then a few.
do m = 1, 5, 4
    allocate(inputs(m), outputs(m))
    do i = 1, m
        inputs(i)%values_ = [real(mod(i, 2)), real(mod(i / 2, 2))]
        outputs(i)%values_ = [real(mod(i, 3)) / 2.]
    end do
    allocate(dcdw(3, 3, nl, m), dcdb(3, nl, m), dcdw_ref(3, 3, nl, m), &
        dcdb_ref(3, nl, m))
    call step(n, w, b, inputs, outputs, dcdw, dcdb, m)
    call step_serial(n, w, b, inputs, outputs, dcdw_ref, dcdb_ref, m)
    if (maxval(abs(dcdw - dcdw_ref)) > 1e-5) error stop "dcdw"
    if (maxval(abs(dcdb - dcdb_ref)) > 1e-5) error stop "dcdb"
    deallocate(inputs, outputs, dcdw, dcdb, dcdw_ref, dcdb_ref)
end do
print *, "ok"
end program
