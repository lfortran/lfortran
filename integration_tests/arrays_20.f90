program arrays_20
    implicit none

    integer, allocatable          :: list_integer(:)
    logical, allocatable          :: list_logical(:)
    character(len=5), allocatable :: list_character(:)

    list_integer   = [integer ::]
    list_logical   = [logical ::]
    list_character = [character(len=len(list_character)) ::]

end program arrays_20
