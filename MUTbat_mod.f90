module machine_constants
    !
    ! Purpose: Define kind parameters for use with declaration statements
    !
    !  e.g.     real(dr)    ::  var2
    !           integer(di) ::  ivar2

    implicit none

	integer, parameter :: dr=selected_real_kind(p=13,r=300)
    integer, parameter :: di=selected_int_kind(9)

end module machine_constants
!----------------------------------------------------------------------
module hsbat_data
    use machine_constants

	! I/O file unit numbers
	logical :: file_open_flag(2100)
	integer :: itmp    ! temporary file unit number
	integer :: ibat   ! scratch_batch (stripped version) file unit number
	integer :: ieco    ! o.eco file unit number

	REAL*8 time_begin, time_end
	REAL*8 total_time_begin, total_time_end

    character(40)       :: prefix
    integer				:: l_prfx
	character(128) :: startdir, newdir, workdir
	integer :: i, status
	logical :: list_of_files

	logical :: do_grok=.true.
	logical :: do_hs=.true.
	logical :: do_hsplot=.true.
	logical :: do_runtime_history=.false.

	character(128) :: grok_path=''
	character(128) :: hs_path=''
	character(128) :: hsplot_path=''

	logical :: scanning, batch_exists
	integer :: len
	
	logical :: create_runtime_file=.false.
	logical :: append_runtime_file=.false.
	integer :: uruntime
	    
    integer, parameter :: nrtmax=100
    integer, parameter :: ntimesmax=100

    character(1028) :: rt_name(nrtmax),rt_data(nrtmax,ntimesmax)
    integer :: nrt, ntimes(nrtmax)


end module hsbat_data
