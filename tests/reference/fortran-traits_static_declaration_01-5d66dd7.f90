module traits_static_declaration_01_m
implicit none
! runtime trait contract ivalue
!   slot 0: ivalue%value

abstract interface :: ivalue
    type(itself), intent(in) :: itself
    integer(4) function value(self) result(r)
        type(itself), intent(in), pass :: self
    end function value
end interface ivalue

end module traits_static_declaration_01_m

program traits_static_declaration_01
implicit none
end program traits_static_declaration_01
