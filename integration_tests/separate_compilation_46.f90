program separate_compilation_46
    use separate_compilation_46a_global, only: global_obj
    use separate_compilation_46a_types, only: obj_t
    implicit none
    allocate(obj_t :: global_obj)
    global_obj%val = 42
    if (global_obj%val /= 42) error stop
    deallocate(global_obj)
    print *, "ok"
end program separate_compilation_46
