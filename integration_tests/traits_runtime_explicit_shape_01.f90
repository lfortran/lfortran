! Explicit-shape array arguments of runtime messages beyond the solver: the
! receiverless messages of IShape, whose slots put the view before the
! dummies that their bounds refer to, take a character array with a lower
! bound, a 2-D block and, in a function, an array that is only copied in;
! IBounds computes bounds from its other dummies and takes integer, logical
! and complex arrays. Each noncontiguous actual is associated by sequence, so
! only its own elements are read and defined.
module traits_runtime_explicit_shape_01_m
    implicit none

    abstract interface :: IShape
        subroutine label(n, names)
            integer, intent(in) :: n
            character(len=3), intent(out) :: names(0:n-1)
        end subroutine label
        subroutine grid(m, n, a)
            integer, intent(in) :: m, n
            real(8), intent(inout) :: a(m, n)
        end subroutine grid
        real(8) function total(n, x)
            integer, intent(in) :: n
            real(8), intent(in) :: x(n)
        end function total
    end interface

    abstract interface :: IBounds
        subroutine fill(self, n, x, y, z)
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            real(8), intent(in) :: x(:)
            integer, intent(inout) :: y(2*n+1)
            real(8), intent(out) :: z(size(x))
        end subroutine fill
        logical function flags(self, n, b, c)
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            logical :: b(n)
            complex(8), intent(inout) :: c(n:2*n-1)
        end function flags
    end interface

    type :: Shape
    end type Shape
    implements IShape :: Shape
        procedure, nopass :: label => shape_label
        procedure, nopass :: grid => shape_grid
        procedure, nopass :: total => shape_total
    end implements

    type :: Bounds
        integer :: offset = 100
    end type Bounds
    implements IBounds :: Bounds
        procedure, pass :: fill => bounds_fill
        procedure, pass :: flags => bounds_flags
    end implements
contains
    subroutine shape_label(n, names)
        integer, intent(in) :: n
        character(len=3), intent(out) :: names(0:n-1)
        integer :: i
        do i = 0, n - 1
            write(names(i), '(a,i2.2)') 'n', i
        end do
    end subroutine shape_label

    subroutine shape_grid(m, n, a)
        integer, intent(in) :: m, n
        real(8), intent(inout) :: a(m, n)
        integer :: i, j
        do j = 1, n
            do i = 1, m
                a(i, j) = a(i, j) + 10 * i + j
            end do
        end do
    end subroutine shape_grid

    real(8) function shape_total(n, x)
        integer, intent(in) :: n
        real(8), intent(in) :: x(n)
        shape_total = sum(x)
    end function shape_total

    subroutine bounds_fill(self, n, x, y, z)
        class(Bounds), intent(in) :: self
        integer, intent(in) :: n
        real(8), intent(in) :: x(:)
        integer, intent(inout) :: y(2*n+1)
        real(8), intent(out) :: z(size(x))
        integer :: i
        do i = 1, 2*n+1
            y(i) = y(i) + self%offset + i
        end do
        z = 2 * x
    end subroutine bounds_fill

    logical function bounds_flags(self, n, b, c)
        class(Bounds), intent(in) :: self
        integer, intent(in) :: n
        logical :: b(n)
        complex(8), intent(inout) :: c(n:2*n-1)
        integer :: i
        b = .not. b
        do i = n, 2*n-1
            c(i) = c(i) * (0d0, 1d0) + i
        end do
        bounds_flags = all(b)
    end function bounds_flags

    ! Through a borrowed read-only view.
    subroutine relabel(shapes, names)
        class(IShape), intent(in) :: shapes
        character(len=3), intent(inout) :: names(:)
        call shapes%label(size(names) / 2, names(size(names):1:-2))
    end subroutine relabel
end module traits_runtime_explicit_shape_01_m

program traits_runtime_explicit_shape_01
    use traits_runtime_explicit_shape_01_m
    implicit none
    class(IShape), allocatable :: s
    class(IBounds), allocatable :: v
    character(len=3) :: names(6)
    real(8) :: a(3, 4), z(6)
    real(8), allocatable :: x(:)
    integer :: iy(10)
    logical :: b(6)
    complex(8) :: c(6)

    allocate(s, source=Shape())
    names = '---'
    call s%label(3, names(5:1:-2))
    if (names(5) /= 'n00' .or. names(3) /= 'n01' .or. names(1) /= 'n02') error stop 1
    if (names(2) /= '---' .or. names(4) /= '---' .or. names(6) /= '---') error stop 2
    names = '---'
    call relabel(Shape(), names)
    if (any(names /= ['---', 'n02', '---', 'n01', '---', 'n00'])) error stop 3
    a = 0
    call s%grid(2, 2, a(2:3, 2:3))
    if (any(a(2:3, 2:3) /= reshape([11d0, 21d0, 12d0, 22d0], [2, 2]))) error stop 4
    if (any(a(1, :) /= 0) .or. any(a(:, 1) /= 0) .or. any(a(:, 4) /= 0)) error stop 5
    allocate(x(5))
    x = [1d0, 100d0, 2d0, 100d0, 3d0]
    if (s%total(3, x(1::2)) + 1 /= 7) error stop 6
    if (any(x /= [1d0, 100d0, 2d0, 100d0, 3d0])) error stop 7

    allocate(v, source=Bounds())
    deallocate(x)
    allocate(x(4))
    x = [1d0, 2d0, 3d0, 4d0]
    iy = 0
    z = -1
    call v%fill(2, x(4:1:-2), iy(10:1:-2), z(1::2))
    if (any(iy /= [0, 105, 0, 104, 0, 103, 0, 102, 0, 101])) error stop 8
    if (any(z /= [8d0, -1d0, 4d0, -1d0, -1d0, -1d0])) error stop 9
    b = [.false., .true., .false., .true., .false., .true.]
    c = [(1d0, 0d0), (2d0, 0d0), (3d0, 0d0), (4d0, 0d0), (5d0, 0d0), (6d0, 0d0)]
    if (.not. v%flags(3, b(1::2), c(2::2))) error stop 10
    if (.not. all(b)) error stop 11
    if (c(2) /= (3d0, 2d0) .or. c(4) /= (4d0, 4d0) .or. c(6) /= (5d0, 6d0)) error stop 12
    if (c(1) /= (1d0, 0d0) .or. c(3) /= (3d0, 0d0) .or. c(5) /= (5d0, 0d0)) error stop 13
    print '(a)', 'traits_runtime_explicit_shape_01 ok'
end program traits_runtime_explicit_shape_01
