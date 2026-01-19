program procedure_27
    procedure(), pointer :: p
    nullify(p)
    if (associated(p)) error stop
end program
