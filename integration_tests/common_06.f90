subroutine solsy()
    double precision rowns
    common /ls0001/ rowns(209)
end

program common_06
    call solsy()
end program
