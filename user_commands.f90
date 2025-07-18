module user_commands

    ! General stuff
    character(120) :: instruction
    logical :: skipping

    character(60), parameter :: skip_on		        =   'skip on'
    character(60), parameter :: skip_off	        =   'skip off'

    character(60), parameter :: pause_file	        =   'pause'

    character(60), parameter :: end_cmd		        =   'end'

    character(60), parameter :: process_dirs	    =   'batch directory list'
    character(60), parameter :: change_dirs			=   'change directory'

    character(60), parameter :: mut_path_cmd		=   'mut path'
    character(60), parameter :: modflow_path_cmd			=   'modflow path'
	character(60), parameter :: MUTPost_path_cmd		=   'MUTplot path'

	character(60), parameter :: do_runtime_history_cmd		=   'runtime history'
	character(60), parameter :: mut_debug_cmd		=   'mut debug'

    contains

	subroutine end_instruction(string)
		use MUTbat_data
		implicit none
		character(*) :: string
		write(ieco,'(a)') string//' EXIT'
		write(*,'(a)') string//' EXIT'
	end subroutine end_instruction


end module user_commands
