module gpu_metal_244_mod
implicit none

type :: shape_t
    integer :: m_
end type

contains

    ! Extent from a BLOCK-local scalar bound once to a struct member
    ! expression -- what an ASSOCIATE name becomes once the offload pass
    ! splices the construct into the kernel body.
    subroutine fill_local_member(a, s, nn)
    real, intent(out) :: a(:,:)
    type(shape_t), intent(in) :: s
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            real, allocatable :: w(:)
            integer :: i, rows
            rows = s%m_ + 2
            allocate(w(rows))
            do i = 1, rows
                w(i) = real(100*jj + i)
            end do
            a(:, jj) = w(1:rows)
        end block
    end do
    end subroutine

    ! Extent from a BLOCK-local scalar bound to a plain scalar dummy.
    subroutine fill_local_scalar(a, mm, nn)
    real, intent(out) :: a(:,:)
    integer, intent(in) :: mm, nn
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            real, allocatable :: w(:)
            integer :: i, rows
            rows = mm + 2
            allocate(w(rows))
            do i = 1, rows
                w(i) = real(200*jj + i)
            end do
            a(:, jj) = w(1:rows)
        end block
    end do
    end subroutine

    ! Chained bindings, as nested ASSOCIATEs leave behind:
    ! associate(base => s%m_) / associate(rows => base + 2).
    subroutine fill_chained(a, s, nn)
    real, intent(out) :: a(:,:)
    type(shape_t), intent(in) :: s
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            real, allocatable :: w(:)
            integer :: i, base, rows
            base = s%m_
            rows = base + 2
            allocate(w(rows))
            do i = 1, rows
                w(i) = real(300*jj + i)
            end do
            a(:, jj) = w(1:rows)
        end block
    end do
    end subroutine

    ! The bound name used in arithmetic in the extent itself.
    subroutine fill_arith(a, s, nn)
    real, intent(out) :: a(:,:)
    type(shape_t), intent(in) :: s
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            real, allocatable :: w(:)
            integer :: i, rows
            rows = s%m_ + 1
            allocate(w(rows + 1))
            do i = 1, rows + 1
                w(i) = real(400*jj + i)
            end do
            a(:, jj) = w(1:rows + 1)
        end block
    end do
    end subroutine

    ! Fence: ASSOCIATE enclosing the loop at procedure scope.
    subroutine fill_assoc_outer(a, s, nn)
    real, intent(out) :: a(:,:)
    type(shape_t), intent(in) :: s
    integer, intent(in) :: nn
    integer :: jj
    associate (rows => s%m_ + 2)
        do concurrent (jj = 1:nn)
            block
                real, allocatable :: w(:)
                integer :: i
                allocate(w(rows))
                do i = 1, rows
                    w(i) = real(500*jj + i)
                end do
                a(:, jj) = w(1:rows)
            end block
        end do
    end associate
    end subroutine

    ! Fence: nested ASSOCIATEs enclosing the loop, as in `formal`.
    subroutine fill_assoc_nested(a, s, nn)
    real, intent(out) :: a(:,:)
    type(shape_t), intent(in) :: s
    integer, intent(in) :: nn
    integer :: jj
    associate (base => s%m_)
        associate (rows => base + 2)
            do concurrent (jj = 1:nn)
                block
                    real, allocatable :: w(:)
                    integer :: i
                    allocate(w(rows))
                    do i = 1, rows
                        w(i) = real(600*jj + i)
                    end do
                    a(:, jj) = w(1:rows)
                end block
            end do
        end associate
    end associate
    end subroutine

    ! Fence: the struct member expression written directly.
    subroutine fill_direct(a, s, nn)
    real, intent(out) :: a(:,:)
    type(shape_t), intent(in) :: s
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            real, allocatable :: w(:)
            integer :: i
            allocate(w(s%m_ + 2))
            do i = 1, s%m_ + 2
                w(i) = real(700*jj + i)
            end do
            a(:, jj) = w(1:s%m_ + 2)
        end block
    end do
    end subroutine

    subroutine check(a, nn, mm, scale)
    real, intent(in) :: a(:,:)
    integer, intent(in) :: nn, mm, scale
    integer :: j, k
    do j = 1, nn
        do k = 1, mm
            if (abs(a(k, j) - real(scale*j + k)) > 1.0e-4) error stop
        end do
    end do
    end subroutine

end module

program gpu_metal_244
use gpu_metal_244_mod
implicit none
integer, parameter :: n = 7, m = 3
real :: out(m, n)
type(shape_t) :: s

s%m_ = m - 2

out = -1
call fill_local_member(out, s, n)
call check(out, n, m, 100)

out = -1
call fill_local_scalar(out, s%m_, n)
call check(out, n, m, 200)

out = -1
call fill_chained(out, s, n)
call check(out, n, m, 300)

out = -1
call fill_arith(out, s, n)
call check(out, n, m, 400)

out = -1
call fill_assoc_outer(out, s, n)
call check(out, n, m, 500)

out = -1
call fill_assoc_nested(out, s, n)
call check(out, n, m, 600)

out = -1
call fill_direct(out, s, n)
call check(out, n, m, 700)

print *, "ok"
end program
