module m
    implicit none
    type :: string_t
        character(:), allocatable :: s
    end type string_t
    type :: string_list_t
        type(string_t), allocatable :: items(:)
    end type string_list_t
contains
    subroutine set_list(list)
        type(string_t), allocatable, intent(in) :: list(:)
        if (.not. allocated(list)) then
            print *, "list is UNALLOCATED"
        else
            print *, "list is allocated with size = ", size(list)
        end if
    end subroutine set_list
end module m

program demo
    use m
    implicit none
    type(string_list_t) :: mylist
    ! This causes the crash because the compiler tries to read 'items' data before passing it
    call set_list(mylist%items)
end program demo