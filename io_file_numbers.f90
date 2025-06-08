subroutine initunit
	use hsbat_data
	implicit none

	file_open_flag(:)=.false.

	! to reserve a unit number uncomment, modify and copy the following line as required
	! file_open_flag(5)=.true.


end subroutine initunit
!----------------------------------------------------------------------
subroutine getunit(iunit)
	use hsbat_data
	implicit none
	integer :: iunit


	iunit=1100
	do

		if(iunit > 2100) stop 'iunit > 2100. Call ghostbusters.'

		if(file_open_flag(iunit)) then
			iunit=iunit+1
			cycle
		else
			file_open_flag(iunit)=.true.
			exit
		endif

	enddo

end subroutine getunit
!----------------------------------------------------------------------
subroutine freeunit(iunit)
	use hsbat_data
	implicit none
	integer :: iunit

    if(file_open_flag(iunit)) then
		file_open_flag(iunit)=.false.
	    close(iunit)
	else
		write(*,*) 'ERROR: unit ',iunit,' already free'
		stop
	endif

end subroutine freeunit
!----------------------------------------------------------------------
subroutine open_ascii_file(iunit,fname)
	implicit none
	integer :: iunit
    character(*) :: fname
    logical :: file_exists
    integer :: status

    inquire(file=fname,exist=file_exists)
    if(.not. file_exists) then
        call input_error(' File not found: '//fname)
    endif

	call getunit(iunit)
    open(iunit,file=fname,form='formatted',iostat=status)
	if(status /= 0) then
		call input_error('Error opening file: '//fname)
	endif

end subroutine open_ascii_file