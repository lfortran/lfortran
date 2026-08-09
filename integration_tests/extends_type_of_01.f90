program extends_type_of_01
implicit none
type nothing
end type nothing

type something_else
end type something_else

type(nothing)         ::  grandpa
type(something_else)  ::  alien

if (extends_type_of(grandpa, alien)) then
    error stop "extends_type_of(grandpa, alien) should be false"
end if

if (same_type_as(grandpa, alien)) then
    error stop "same_type_as(grandpa, alien) should be false"
end if

if (.not. extends_type_of(grandpa, grandpa)) then
    error stop "extends_type_of(grandpa, grandpa) should be true"
end if

if (.not. same_type_as(grandpa, grandpa)) then
    error stop "same_type_as(grandpa, grandpa) should be true"
end if

print *, "OK"
end program extends_type_of_01