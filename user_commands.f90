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

    character(60), parameter :: grok_path_cmd		=   'grok path'
    character(60), parameter :: hs_path_cmd			=   'hydrogeosphere path'
	character(60), parameter :: hsplot_path_cmd		=   'hsplot path'

	character(60), parameter :: do_runtime_history_cmd		=   'runtime history'

    contains

	subroutine end_instruction(string)
		use hsbat_data
		implicit none
		character(*) :: string
		write(ieco,'(a)') string//' EXIT'
		write(*,'(a)') string//' EXIT'
	end subroutine end_instruction


end module user_commands
