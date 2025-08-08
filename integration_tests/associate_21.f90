program associate_21
    use associate_21_mod_b
    type(model_t) :: tmp_model
    allocate(tmp_model%dependency(1))
    call tmp_model%update_dependency(1, 5)

    !!! TODO: Need to associate properly and handle integer pointer
    ! if (tmp_model%dependency(1)%name /= "LFortran") error stop   
    ! if (tmp_model%dependency(1)%key /= 5) error stop
end program