program namelist_28
    ! Test for handling null values with comma separators
    implicit none

    character(len=15), allocatable :: name(:)
    integer :: count
    namelist /expected/ name, count
    character(len=256) :: readme(4)
    integer :: i

    ! Test null value for array with comma separator before next variable
    allocate(name(10))
    name = [(repeat(' ', len(name)), i=1, size(name))]
    count = 0

    readme(1) = '&EXPECTED'
    readme(2) = ' NAME=,'
    readme(3) = ' COUNT=42'
    readme(4) = ' /'

    read(readme, nml=expected)
    print *, 'Test 1: NAME should remain blank, COUNT should be 42'

    if (trim(name(1)) /= '') then
        error stop 'NAME should be blank (null value)'
    end if
    if (count /= 42) then
        error stop 'COUNT should be 42'
    end if

    ! Test null value at end with comma
    count = 0
    readme(1) = '&EXPECTED'
    readme(2) = ' COUNT=99,'
    readme(3) = ' NAME=,'
    readme(4) = ' /'

    read(readme, nml=expected)
    print *, 'Test 2: COUNT should be 99, NAME should remain blank'

    if (count /= 99) then
        error stop 'COUNT should be 99'
    end if
    if (trim(name(1)) /= '') then
        error stop 'NAME should be blank (null value) in test 2'
    end if

    ! Test multiple values with commas
    count = 0
    readme(1) = '&EXPECTED'
    readme(2) = ' COUNT=123, NAME="test"'
    readme(3) = ' /'
    readme(4) = ' '

    read(readme, nml=expected)
    print *, 'Test 3: COUNT should be 123, NAME(1) should be "test"'

    if (count /= 123) then
        error stop 'COUNT should be 123 in test 3'
    end if
    if (trim(name(1)) /= 'test') then
        error stop 'NAME(1) should be "test" in test 3'
    end if

    print *, 'OK: All tests passed'

end program namelist_28
