program format_58 
    implicit none 
    integer,parameter:: sp = kind(1e0), dp = kind(1d0),&
        sep = selected_real_kind(precision(1.0_dp)+1),&
        ep = (sep+dp+sign(1,sep)*(sep-dp))/2 
    character(11) values(3)
    real(sp),parameter:: xsp = 1
    real(dp),parameter:: xdp = 1 
    real(ep),parameter:: xep = 1
    write(*,"(A9,3A12)") 'kind name','sp','dp','ep'
    write(*,"(A9,3I12)") 'kind:',sp, dp, ep
    values = (/ es(real(epsilon(xsp),ep),11,2,4), & 
        es(real(epsilon(xdp),ep),11,2,4),es(real(epsilon(xep),ep),11,2,4) /)
    write(*,"(4(1X,A))") 'epsilon:',values
    values = (/ es(real(   tiny(xsp),ep),11,2,4), &
        es(real(   tiny(xdp),ep),11,2,4),es(real(   tiny(xep),ep),11,2,4) /)
    write(*,"(4(1X,A))") '   tiny:',values
    values = (/ es(real(   huge(xsp),ep),11,2,4), &
        es(real(   huge(xdp),ep),11,2,4),es(real(   huge(xep),ep),11,2,4) /)
    write(*,"(4(1X,A))") '   huge:',values
contains
    function cn(n)
        integer,intent(in)::n
        character(range(n)+2) cn
        write(cn,"(I0)") n
        cn = adjustl(cn)
    end function cn
    
    function es(x,w,d,e)
        real(ep),intent(in):: x
        integer,intent(in) :: w,d,e
        character(w) es, ce, c_aftere, ctenpower, csigfigs
        character(1) cfirstdigit, cesign, csignx, claterdigits*(d+1)
        real(ep) signx, absx, scaledx
        integer pointplace,eplace,digitsaftere,tenpower,k 
        signx = sign(1.0_ep,x)
        csignx = merge(' ','-',signx>0.0_ep)

        if(x==0.0_ep)then
        es = adjustr(csignx//'0.'//repeat('0',d)//'E+'//repeat('0',e))
        return
        end if

        absx = abs(x)
        k = int(log10(absx))
        scaledx = x/10.0_ep**k
        write(ce,"(E"//trim(cn(w))//"."//trim(cn(d+1))//")") scaledx
        eplace = index(ce,'E')
        pointplace = index(ce,'.')
        csigfigs = adjustl(ce(pointplace+1:eplace-1))
        c_aftere = adjustl(trim(ce(eplace+1:)))
        read(c_aftere,*) digitsaftere
        tenpower = digitsaftere + k - 1
        cesign = merge('+','-',tenpower>=0)
        ctenpower = adjustl(trim(cn(abs(tenpower))))
        cfirstdigit = csigfigs(1:1)
        claterdigits = csigfigs(2:)
        es = csignx//cfirstdigit//'.'//trim(claterdigits)//'E'//cesign// &
            repeat('0',e-len_trim(ctenpower))//trim(ctenpower)
    end function es
end program format_58
