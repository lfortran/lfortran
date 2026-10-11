! BLOCK DATA in a file of its own. It initializes blank COMMON (an extension,
! which GFortran also accepts) and a named block spelled in upper case.
block data common_57_data
    implicit none
    real :: r(2)
    integer :: i, j, k
    common r
    COMMON /COMMON_57_BLK/ i, j, k
    data r / 1.5, 2.5 /
    data i, j, k / 3, 0, 4 /
end block data common_57_data
