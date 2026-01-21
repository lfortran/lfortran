program namelist_multi_groups
    implicit none

    ! Variables for first group
    integer :: a, b

    ! Variables for second group
    real :: x, y

    ! Variables for third group
    character(len=10) :: name1, name2

    ! Define three separate namelist groups
    namelist /group1/ a, b
    namelist /group2/ x, y
    namelist /group3/ name1, name2

    ! Initialize group1 variables
    a = 100
    b = 200

    ! Initialize group2 variables
    x = 1.5
    y = 2.5

    ! Initialize group3 variables
    name1 = 'hello'
    name2 = 'world'

    ! Write each group to separate files
    open(unit=11, file='namelist_group1.dat', status='replace', form='formatted')
    write(11, nml=group1)
    close(11)

    open(unit=12, file='namelist_group2.dat', status='replace', form='formatted')
    write(12, nml=group2)
    close(12)

    open(unit=13, file='namelist_group3.dat', status='replace', form='formatted')
    write(13, nml=group3)
    close(13)

    ! Reset all variables
    a = 0
    b = 0
    x = 0.0
    y = 0.0
    name1 = ''
    name2 = ''

    ! Read each group from respective files
    open(unit=11, file='namelist_group1.dat', status='old', form='formatted')
    read(11, nml=group1)
    close(11)

    open(unit=12, file='namelist_group2.dat', status='old', form='formatted')
    read(12, nml=group2)
    close(12)

    open(unit=13, file='namelist_group3.dat', status='old', form='formatted')
    read(13, nml=group3)
    close(13)

    ! Verify group1 values
    if (a /= 100) error stop "Variable a mismatch"
    if (b /= 200) error stop "Variable b mismatch"

    ! Verify group2 values
    if (abs(x - 1.5) > 1.0e-5) error stop "Variable x mismatch"
    if (abs(y - 2.5) > 1.0e-5) error stop "Variable y mismatch"

    ! Verify group3 values
    if (trim(name1) /= 'hello') error stop "Variable name1 mismatch"
    if (trim(name2) /= 'world') error stop "Variable name2 mismatch"

    print *, "Multiple namelist groups test passed!"

end program namelist_multi_groups
