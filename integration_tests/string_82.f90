program string_82
    class(*), allocatable :: x
    allocate(x, source='abc')
    select type (x)
    type is (character(len=*))
        print *, "String type"
        ! print *, x ! <<<<<<<<<<<<<<<<<< unlimited polymorphic types not supported yet
    class default
        print *, "Unknown type"
        error stop
    end select
end program