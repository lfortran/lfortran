! An elemental intrinsic of a zero-size constant array is a zero-size array
! constant. A statement that uses it still runs: an assignment allocates
! its variable with size zero, and a call or an output statement is kept.
program empty_array_06
    implicit none
    integer, parameter :: pe(0) = [integer :: ]
    real, parameter :: pr(0) = [real :: ]
    integer, parameter :: p2(0, 3) = reshape([integer :: ], [0, 3])
    character(len=3), parameter :: ce(0) = [character(len=3) :: ]
    real(8), allocatable :: x(:)
    real, allocatable :: y(:)
    integer, allocatable :: k(:), k2(:, :)
    character(len=:), allocatable :: cs(:)
    character(len=8) :: line
    integer :: ncalls

    allocate(x(3))
    x = dble(pe)
    if (size(x) /= 0) error stop
    allocate(y(3))
    y = float(pe)
    if (size(y) /= 0) error stop
    deallocate(x)
    allocate(x(3))
    x = dfloat(pe)
    if (size(x) /= 0) error stop
    deallocate(x)
    allocate(x(3))
    x = dble(pr)
    if (size(x) /= 0) error stop
    deallocate(x)
    allocate(x(3))
    x = dble([integer :: ])
    if (size(x) /= 0) error stop
    deallocate(x)
    allocate(x(3))
    x = real(pe, 8)
    if (size(x) /= 0) error stop
    deallocate(x)
    allocate(x(3))
    x = real([integer :: ], 8)
    if (size(x) /= 0) error stop
    allocate(k(3))
    k = abs(pe)
    if (size(k) /= 0) error stop
    deallocate(k)
    allocate(k(3))
    k = abs([integer :: ])
    if (size(k) /= 0) error stop
    allocate(k2(2, 2))
    k2 = abs(p2)
    if (size(k2, 1) /= 0 .or. size(k2, 2) /= 3) error stop
    allocate(character(len=3) :: cs(2))
    cs = adjustl(ce)
    if (size(cs) /= 0) error stop

    line = "x"
    write(line, "(i0)") size(dble(pe))
    if (line /= "0") error stop
    line = "x"
    write(line, "(i0)") size(float([integer :: ]))
    if (line /= "0") error stop
    line = "x"
    write(line, "(i0)") size(abs([integer :: ]))
    if (line /= "0") error stop
    line = "x"
    write(line, "(i0)") size(real(pe, 8))
    if (line /= "0") error stop

    ncalls = 0
    call check_empty(dble(pe))
    call check_empty(dfloat([integer :: ]))
    call check_empty(real(pe, 8))
    call check_empty(real(abs(pe), 8))
    if (ncalls /= 4) error stop

    print *, size(dble(pe)), size(abs([integer :: ]))

contains

    subroutine check_empty(a)
        real(8), intent(in) :: a(:)
        if (size(a) /= 0) error stop
        ncalls = ncalls + 1
    end subroutine

end program empty_array_06
