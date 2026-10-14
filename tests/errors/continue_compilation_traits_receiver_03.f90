! Explicit trait receivers (TD-36): a POINTER statement that names the
! receiver, before or after its declaration with the PASS attribute, makes
! it a pointer, which a receiver is not (F2018 C760). A standalone POINTER
! statement needs --implicit-interface, so these cases continue
! continue_compilation_traits_receiver_01.f90 here. Each module whose name
! contains _bad_ has exactly one error.

! R35: a POINTER statement before the declaration of the receiver.
module cc_traits_receiver_bad_r35
    implicit none
    abstract interface :: IR35
        integer function f(self)
            pointer :: self
            type(itself), intent(in), pass :: self
        end function
    end interface
end module cc_traits_receiver_bad_r35

! R36: and one after it.
module cc_traits_receiver_bad_r36
    implicit none
    abstract interface :: IR36
        integer function f(self)
            type(itself), intent(in), pass :: self
            pointer :: self
        end function
    end interface
end module cc_traits_receiver_bad_r36
