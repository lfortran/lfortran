! Type-level size(matmul(...)) and size(transpose(...)) must follow the
! result shape, not the size of the first array argument.
program array_size_matmul_01
implicit none
integer, parameter :: nrow = 3, ncol = 2, ncb = 4
real :: a(nrow, ncol), b(ncol, ncb), v(ncol)
integer :: i, k

do i = 1, nrow
    do k = 1, ncol
        a(i, k) = real(i) + 10.0 * real(k)
    end do
end do
do i = 1, ncol
    do k = 1, ncb
        b(i, k) = real(i) * 2.0 - real(k)
    end do
end do
do i = 1, ncol
    v(i) = real(i) * 3.0 - 1.0
end do

! (n,k) x (k) -> (n): size is n, not n*k.
if (auto_matvec(a, v) /= nrow) error stop "matvec extent"
! (n,k) x (k,m) -> (n,m)
if (.not. auto_matmat(a, b, nrow, ncb)) error stop "matmat extents"
! transpose swaps the two extents.
if (.not. auto_transpose(a, ncol, nrow)) error stop "transpose extents"
! Runtime dim selects among those extents via merge.
if (auto_matmat_dim(a, b, 1) /= nrow) error stop "matmat dim 1"
if (auto_matmat_dim(a, b, 2) /= ncb) error stop "matmat dim 2"
! The same queries in an executable statement rather than in a declaration:
! these go down the general (non type-level) path, which must agree.
if (.not. runtime_extents(a, b, v)) error stop "runtime extents"

contains

function auto_matvec(x, y) result(n)
real, intent(in) :: x(:,:), y(:)
integer :: n
real :: t(size(matmul(x, y)))
n = size(t)
end function

function auto_matmat(x, y, d1, d2) result(ok)
real, intent(in) :: x(:,:), y(:,:)
integer, intent(in) :: d1, d2
logical :: ok
real :: t(size(matmul(x, y), 1), size(matmul(x, y), 2))
t = matmul(x, y)
ok = size(t, 1) == d1 .and. size(t, 2) == d2 .and. size(t) == d1 * d2
end function

function auto_transpose(x, d1, d2) result(ok)
real, intent(in) :: x(:,:)
integer, intent(in) :: d1, d2
logical :: ok
real :: t(size(transpose(x), 1), size(transpose(x), 2))
t = transpose(x)
ok = size(t, 1) == d1 .and. size(t, 2) == d2
end function

! size(...) asked outside a declaration, where the extent is computed
! rather than used to shape an object.
function runtime_extents(x, y, z) result(ok)
real, intent(in) :: x(:,:), y(:,:), z(:)
logical :: ok
ok = size(matmul(x, y)) == size(x, 1) * size(y, 2) &
    .and. size(matmul(x, y), 1) == size(x, 1) &
    .and. size(matmul(x, y), 2) == size(y, 2) &
    .and. size(matmul(x, z)) == size(x, 1) &
    .and. size(matmul(z, y)) == size(y, 2) &
    .and. size(transpose(x), 1) == size(x, 2) &
    .and. size(transpose(x), 2) == size(x, 1)
end function

function auto_matmat_dim(x, y, d) result(n)
real, intent(in) :: x(:,:), y(:,:)
integer, intent(in) :: d
integer :: n
real :: t(size(matmul(x, y), d))
n = size(t)
end function

end program
