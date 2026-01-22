module select_type_19_m
    implicit none
contains
    subroutine takes_logical_array(a)
        logical, intent(in) :: a(:)
        if (count(a) /= 2) error stop
    end subroutine takes_logical_array
end module select_type_19_m

program select_type_19
    use select_type_19_m, only: takes_logical_array
    implicit none

    logical, target :: l(3)
    class(*), pointer :: generic(:)

    l = [.true., .false., .true.]
    generic => l

    select type (generic)
    type is (logical)
        call takes_logical_array(generic)
    class default
        error stop
    end select

    print *, "ok"
end program select_type_19
