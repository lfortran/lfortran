subroutine dinvr()
    call dinvr_main__lcompilers(1)
end subroutine dinvr
subroutine dinvr_main__lcompilers(entry__lcompilers)
    integer(4) :: entry__lcompilers
    real(4) :: xlb
    if (entry__lcompilers == 1) then
        go to 1
    end if
    if (entry__lcompilers == 2) then
        go to 2
    end if
1 continue
    call dstzr(xlb)
    return
2 continue
    return
    return
end subroutine dinvr_main__lcompilers
subroutine dstinv()
    call dinvr_main__lcompilers(2)
end subroutine dstinv
