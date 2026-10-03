program arrays_108
implicit none

type :: str_array
    character(len = :), allocatable :: strings(:)
end type str_array

type(str_array) :: array_of_arrays(1)
character(len = :), allocatable :: array(:)

allocate(character(3) :: array_of_arrays(1)%strings(3))
array_of_arrays(1)%strings(1) = 'AAA'
array_of_arrays(1)%strings(2) = 'BBB'
array_of_arrays(1)%strings(3) = 'CCC'

allocate(array(1), mold = array_of_arrays(1)%strings(1))

if (len(array(1)) /= 3) error stop
print *, "PASSED"
end program
