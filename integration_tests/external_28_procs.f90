integer function q()
    q = 2
end function q

subroutine s(y)
    integer, intent(out) :: y
    y = 20
end subroutine s

integer function use_q()
    interface
        integer function q()
        end function q
    end interface
    use_q = q()
end function use_q

subroutine use_s(y)
    integer, intent(out) :: y
    interface
        subroutine s(y)
            integer, intent(out) :: y
        end subroutine s
    end interface
    call s(y)
end subroutine use_s
