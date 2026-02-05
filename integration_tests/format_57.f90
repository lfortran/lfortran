
program format_57
    implicit none
    integer,parameter:: sp = kind(1e0), dp = kind(1d0),&
        sep = selected_real_kind(precision(1.0_dp)+1),&
        ep = merge(sep,dp,sep>0)
    real(sp),parameter:: xsp = 1
    real(dp),parameter:: xdp = 1 
    real(ep),parameter:: xep = 1
    write(*,"(A)")            'kind             sp         dp         ep'
    write(*,"(A,3(E12.3E4))")'epsilon:',epsilon(xsp),epsilon(xdp),epsilon(xep)
    write(*,"(A,3(E12.3E4))")'tiny   :',   tiny(xsp),   tiny(xdp),   tiny(xep)
    write(*,"(A,3(E12.3E4))")'huge   :',   huge(xsp),   huge(xdp),   huge(xep)
    write(*,"(A,3(ES11.2E4))")'epsilon:',epsilon(xsp),epsilon(xdp),epsilon(xep)
    write(*,"(A,3(ES11.2E4))")'tiny   :',   tiny(xsp),   tiny(xdp),   tiny(xep)
    write(*,"(A,3(ES11.2E4))")'huge   :',   huge(xsp),   huge(xdp),   huge(xep)
    print "(A,E 0.5,A)", '"',2/9.0d200,'"'
    print "(A,E12.5,A)", '"',2/9.0d200,'"'
    print "(A,ES 0.5,A)", '"',2/9.0d200,'"'
    print "(A,ES12.5,A)", '"',2/9.0d200,'"'
end program format_57
