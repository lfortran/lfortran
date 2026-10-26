program read_119
    ! A READ into a component of an array of derived types fills the component
    ! of every element of the array, and leaves the other components of those
    ! elements alone (#15824, #15407, #14257).
    implicit none
    type t
        integer :: k
        integer :: j
    end type
    type(t) :: x(3)
    character(len=32) :: s

    x%j = [-1, -1, -1]
    s = ' 1 2 3'
    read(s, '(3I2)') x%k
    if (x(1)%k /= 1) error stop 1
    if (x(2)%k /= 2) error stop 2
    if (x(3)%k /= 3) error stop 3
    ! The other component of the same elements is left alone.
    if (x(1)%j /= -1) error stop 4
    if (x(2)%j /= -1) error stop 5
    if (x(3)%j /= -1) error stop 6

    ! The same, list-directed.
    x%j = [-1, -1, -1]
    s = ' 4 5 6'
    read(s, *) x%k
    if (x(1)%k /= 4) error stop 7
    if (x(2)%k /= 5) error stop 8
    if (x(3)%k /= 6) error stop 9
    if (x(1)%j /= -1) error stop 10
    if (x(2)%j /= -1) error stop 11
    if (x(3)%j /= -1) error stop 12

    ! An allocatable array of derived types.
    block
        type(t), allocatable :: y(:)
        allocate(y(3))
        y%j = [-1, -1, -1]
        s = ' 7 8 9'
        read(s, '(3I2)') y%k
        if (y(1)%k /= 7) error stop 13
        if (y(2)%k /= 8) error stop 14
        if (y(3)%k /= 9) error stop 15
        if (y(1)%j /= -1) error stop 16
        if (y(2)%j /= -1) error stop 17
        if (y(3)%j /= -1) error stop 18
    end block

    print *, 'ok'
end program read_119
