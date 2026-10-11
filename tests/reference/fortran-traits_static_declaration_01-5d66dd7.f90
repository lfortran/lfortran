module traits_static_declaration_01_m
implicit none
! runtime trait contract ivalue
!   slot 0: ivalue%value

abstract interface :: ivalue
    integer(4) function value() result(r)
    end function value
end interface ivalue

end module traits_static_declaration_01_m

program traits_static_declaration_01
implicit none
end program traits_static_declaration_01
