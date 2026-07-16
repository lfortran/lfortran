program coarray_var_name_collision_01
    implicit none
    integer :: stat
    stat = 0
    sync all(stat=stat)
    print *, stat
end program coarray_var_name_collision_01
