module traits_runtime_inspection_kind_oracle_m
    implicit none
    type :: KindBox(k)
        integer, kind :: k = 4
        integer :: n
    end type
end module

program traits_runtime_inspection_04_oracle
    use traits_runtime_inspection_kind_oracle_m
    implicit none
    type(KindBox(4)), target :: first
    type(KindBox(8)), target :: second
    class(*), pointer :: view
    integer :: i
    first%n = 23
    second%n = 29
    if (storage_size(first) /= storage_size(second)) error stop 1
    do i = 1, 2
        if (i == 1) then
            view => first
        else
            view => second
        end if
        select type (concrete => view)
        type is (KindBox(4))
            if (i /= 1 .or. concrete%n /= 23) error stop 2
        type is (KindBox(8))
            if (i /= 2 .or. concrete%n /= 29) error stop 3
        class default
            error stop 4
        end select
    end do
end program
