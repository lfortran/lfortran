MODULE struct_type_01_module

TYPE diag_type
    INTEGER :: len
END TYPE diag_type

TYPE(diag_type), DIMENSION(1) :: diag

END MODULE struct_type_01_module

subroutine set_diag_len(len)
USE struct_type_01_module, ONLY: diag
INTEGER, INTENT(IN) :: len

print *, diag(1)%len
if (diag(1)%len /= 10) error stop

diag(1)%len = len

end subroutine set_diag_len

PROGRAM struct_type_01
USE struct_type_01_module, ONLY: diag

diag(1)%len = 10
call set_diag_len(20)
print *, get_diag_len()
if (get_diag_len() /= 20) error stop

contains
function get_diag_len() result(len)
USE struct_type_01_module, ONLY: diag
INTEGER :: len

len = diag(1)%len
end function get_diag_len

END PROGRAM struct_type_01

