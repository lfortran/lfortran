program read_109
    ! A formatted READ with a G edit into integer arrays and with B/O/Z edits
    ! into real and complex items stores every element with its own size,
    ! and leaves the neighbouring variables unchanged.
    implicit none
    integer(1) :: i1(4), i1_gap(3)
    integer(2) :: i2(4)
    integer(4) :: i4(4)
    integer(8) :: i8(4)
    integer(1), allocatable :: a1(:)
    integer(2), allocatable :: a2(:)
    integer(4), allocatable :: a4(:)
    integer(8), allocatable :: a8(:)
    integer(1), target :: t1(7)
    integer(2), target :: t2(7)
    integer(4), target :: t4(7)
    integer(8), target :: t8(7)
    integer(1), pointer :: p1(:)
    integer(2), pointer :: p2(:)
    integer(4), pointer :: p4(:)
    integer(8), pointer :: p8(:)
    real(4) :: r4, r4e(3), r4a(2)
    real(8) :: r8, r8e(3), r8a(2)
    complex(4) :: c4, c4a(2)
    complex(8) :: c8, c8a(2)
    real(4), allocatable :: r4l(:)
    real(8), allocatable :: r8l(:)
    complex(8), allocatable :: c8l(:)
    real(4), target :: rt4(5)
    real(8), target :: rt8(5)
    real(4), pointer :: rp4(:)
    real(8), pointer :: rp8(:)
    character(len=12) :: s
    character(len=64) :: z
    integer :: ios

    s = '  1  2  3  4'

    ! G into fixed-size integer arrays
    i1 = -1; i1_gap = 9
    read(s, '(4G3.0)', iostat=ios) i1
    if (ios /= 0 .or. any(i1 /= [1, 2, 3, 4]) .or. any(i1_gap /= 9)) error stop 1
    i2 = -1
    read(s, '(4G3.0)', iostat=ios) i2
    if (ios /= 0 .or. any(i2 /= [1, 2, 3, 4])) error stop 2
    i4 = -1
    read(s, '(4G3.0)') i4
    if (any(i4 /= [1, 2, 3, 4])) error stop 3
    i8 = -1
    read(s, '(4G3.0)', iostat=ios) i8
    if (ios /= 0 .or. any(i8 /= [1, 2, 3, 4])) error stop 4

    ! G into allocatable integer arrays
    allocate(a1(4), a2(4), a4(4), a8(4))
    a1 = -1; a2 = -1; a4 = -1; a8 = -1
    read(s, '(4G3.0)', iostat=ios) a1
    if (ios /= 0 .or. any(a1 /= [1, 2, 3, 4])) error stop 5
    read(s, '(4G3.0)', iostat=ios) a2
    if (ios /= 0 .or. any(a2 /= [1, 2, 3, 4])) error stop 6
    read(s, '(4G3.0)', iostat=ios) a4
    if (ios /= 0 .or. any(a4 /= [1, 2, 3, 4])) error stop 7
    read(s, '(4G3.0)', iostat=ios) a8
    if (ios /= 0 .or. any(a8 /= [1, 2, 3, 4])) error stop 8

    ! G into strided pointer integer arrays; the gaps stay unchanged
    t1 = -1; t2 = -1; t4 = -1; t8 = -1
    p1 => t1(1:7:2); p2 => t2(1:7:2); p4 => t4(1:7:2); p8 => t8(1:7:2)
    read(s, '(4G3.0)', iostat=ios) p1
    if (ios /= 0 .or. any(t1 /= [1, -1, 2, -1, 3, -1, 4])) error stop 9
    read(s, '(4G3.0)', iostat=ios) p2
    if (ios /= 0 .or. any(t2 /= [1, -1, 2, -1, 3, -1, 4])) error stop 10
    read(s, '(4G3.0)', iostat=ios) p4
    if (ios /= 0 .or. any(t4 /= [1, -1, 2, -1, 3, -1, 4])) error stop 11
    read(s, '(4G3.0)', iostat=ios) p8
    if (ios /= 0 .or. any(t8 /= [1, -1, 2, -1, 3, -1, 4])) error stop 12

    ! An integer array started with I and continued with G
    i4 = -1
    read(s, '(I3, 3G3.0)', iostat=ios) i4
    if (ios /= 0 .or. any(i4 /= [1, 2, 3, 4])) error stop 13

    ! B/O/Z into real and complex scalars
    z = '3F800000'
    r4 = 0
    read(z, '(Z8)', iostat=ios) r4
    if (ios /= 0 .or. r4 /= 1.0) error stop 14
    z = '00111111100000000000000000000000'
    r4 = 0
    read(z, '(B32)', iostat=ios) r4
    if (ios /= 0 .or. r4 /= 1.0) error stop 15
    z = '3FF0000000000000'
    r8 = 0
    read(z, '(Z16)', iostat=ios) r8
    if (ios /= 0 .or. r8 /= 1.0d0) error stop 16
    z = '400000000000000000000'
    r8 = 0
    read(z, '(O22)', iostat=ios) r8
    if (ios /= 0 .or. r8 /= 2.0d0) error stop 17
    z = '3F80000040000000'
    c4 = 0
    read(z, '(2Z8)', iostat=ios) c4
    if (ios /= 0 .or. c4 /= (1.0, 2.0)) error stop 18
    z = '3FF00000000000004000000000000000'
    c8 = 0
    read(z, '(2Z16)', iostat=ios) c8
    if (ios /= 0 .or. c8 /= (1.0d0, 2.0d0)) error stop 19

    ! B/O/Z into real elements; the next element stays unchanged
    z = '40000000'
    r4e = -1
    read(z, '(Z8)', iostat=ios) r4e(2)
    if (ios /= 0 .or. any(r4e /= [-1.0, 2.0, -1.0])) error stop 20
    z = '4000000000000000'
    r8e = -1
    read(z, '(Z16)', iostat=ios) r8e(2)
    if (ios /= 0 .or. any(r8e /= [-1.0d0, 2.0d0, -1.0d0])) error stop 21

    ! B/O/Z into fixed-size real and complex arrays
    z = '3F80000040000000'
    r4a = 0
    read(z, '(2Z8)', iostat=ios) r4a
    if (ios /= 0 .or. any(r4a /= [1.0, 2.0])) error stop 22
    z = '3FF00000000000004000000000000000'
    r8a = 0
    read(z, '(2Z16)', iostat=ios) r8a
    if (ios /= 0 .or. any(r8a /= [1.0d0, 2.0d0])) error stop 23
    z = '3F800000400000004040000040800000'
    c4a = 0
    read(z, '(4Z8)', iostat=ios) c4a
    if (ios /= 0 .or. any(c4a /= [(1.0, 2.0), (3.0, 4.0)])) error stop 24
    z = '3FF0000000000000400000000000000040080000000000004010000000000000'
    c8a = 0
    read(z, '(4Z16)', iostat=ios) c8a
    if (ios /= 0 .or. any(c8a /= [(1.0d0, 2.0d0), (3.0d0, 4.0d0)])) error stop 25

    ! B/O/Z into allocatable real and complex arrays
    allocate(r4l(2), r8l(2), c8l(1))
    z = '3F80000040000000'
    r4l = 0
    read(z, '(2Z8)', iostat=ios) r4l
    if (ios /= 0 .or. any(r4l /= [1.0, 2.0])) error stop 26
    z = '377600000000000000000 400000000000000000000'
    r8l = 0
    read(z, '(O21, 1X, O21)', iostat=ios) r8l
    if (ios /= 0 .or. any(r8l /= [1.0d0, 2.0d0])) error stop 27
    z = '3FF00000000000004000000000000000'
    c8l = 0
    read(z, '(2Z16)', iostat=ios) c8l
    if (ios /= 0 .or. c8l(1) /= (1.0d0, 2.0d0)) error stop 28

    ! B/O/Z into strided pointer real arrays; the gaps stay unchanged
    rt4 = -1
    rp4 => rt4(1:5:2)
    z = '3F8000004000000040400000'
    read(z, '(3Z8)', iostat=ios) rp4
    if (ios /= 0 .or. any(rt4 /= [1.0, -1.0, 2.0, -1.0, 3.0])) error stop 29
    rt8 = -1
    rp8 => rt8(1:5:2)
    z = '3FF000000000000040000000000000004008000000000000'
    read(z, '(3Z16)', iostat=ios) rp8
    if (ios /= 0 .or. any(rt8 /= [1.0d0, -1.0d0, 2.0d0, -1.0d0, 3.0d0])) error stop 30

    print *, 'ok'
end program read_109
