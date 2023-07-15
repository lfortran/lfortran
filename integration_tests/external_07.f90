subroutine dqc25f()
    external f
    call dqk15w(f)
end
program external_07
    call dqc25f()
end program