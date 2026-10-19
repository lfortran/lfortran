program read_110
    ! A bad Z field for the imaginary part of a complex array item, without
    ! iostat=. LFortran goes on with the READ (#15081; gfortran stops with an
    ! error, hence no gfortran label). The next array item, started by I and
    ! continued by G, must still be read into its own elements. Delete this
    ! test once #15081 is fixed and the READ terminates at the error.
    implicit none
    type t
        complex :: c(1)
        integer :: w(2)
        integer :: guard
    end type
    type(t) :: v
    character(len=16) :: lines(2)
    lines(1) = '3F800000XXXXXXXX'
    lines(2) = '  1  2'
    v%c = (-1, -1)
    v%w = -1
    v%guard = -1
    read(lines, '(Z8,Z8,(I3,G3.0))') v%c, v%w
    if (real(v%c(1)) /= 1) error stop 1
    if (any(v%w /= [1, 2])) error stop 2
    if (v%guard /= -1) error stop 3
    print *, 'ok'
end program read_110
