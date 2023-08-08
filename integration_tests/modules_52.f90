module module_52_fpm_cmd_new

contains

subroutine validate_toml_data(input)

use module_52_tomlf_de, only : toml_parse
use module_52_tomlf_type, only : toml_table
implicit none
character(len=:), intent(in), allocatable :: input(:)
type(toml_table), allocatable :: table

if (allocated(table)) deallocate(table)
call toml_parse(table, input)

end subroutine validate_toml_data

end module module_52_fpm_cmd_new
