module arrays_50_mod_b
    integer ::j=20
    integer ::j2=30
     
end module arrays_50_mod_b

module arrays_50_mod_a
    contains
    subroutine fdf()
        use arrays_50_mod_b
        integer :: o(j,j2)                                                                                                                                                                                                                                         
        print *,j,j2
        if (j /= 20) error stop
        if (j2 /= 30) error stop
        print *, size(o)    
        if (size(o) /= 600) error stop
    end subroutine fdf
end module arrays_50_mod_a

