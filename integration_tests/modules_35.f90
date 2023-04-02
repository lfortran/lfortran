module M_CLI2_21

contains

subroutine get_args_fixed_length_a_array(strings)
character(len=*), allocatable :: strings(:)
character(len=:), allocatable :: strings_a(:)

integer :: place
if(place > 0) then
    if (len(strings_a) <= len(strings))then
        strings = strings_a
    else
        strings = [character(len=len(strings)) ::]
    end if
end if

if(place > 0) then
    strings = [character(len=len(strings)) ::]
end if
end subroutine get_args_fixed_length_a_array

end module M_CLI2_21

program modules_35
    use M_CLI2_21
    character(len=5), allocatable :: string(:)
    allocate(string(2))

    print *, "executing modules_35"
    call get_args_fixed_length_a_array(string)
end program
