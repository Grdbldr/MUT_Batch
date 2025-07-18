!----------------------------------------------------------------------
program MUTbatch_processor
	use MUTbat_data
	use user_commands
	use dfport
	implicit none

	real :: dummy,dummy2
	character(4096) :: line
	logical :: file_exists, found

    character(128) ::lfile,  local_dir
    

    real(dr) :: decyear, dyear
	CHARACTER(8) :: curdate
	CHARACTER(10) :: curtime

	CALL DATE_AND_TIME (curdate, curtime)
	
	dyear=decyear(curdate, curtime)

    lfile='local_runtime.dat'


	dummy2=secnds(0.0)

	ieco=0
    call header(ieco)

	! Open a file containing pathnames to MUT data sets
	itmp=55
    call enter_prefix(prefix,l_prfx,itmp,'.MUTbatch')

    ibat=56
	open(ibat,file='scratch_batch',status='unknown')
    call strip_comments(itmp,ibat)
	close(itmp)

	status = getcwd (startdir)

	scanning=.true.

    write(ieco,*) 'PASS1 Scan Input'

    1000 continue

    call header(ieco)

	len=len_trim(startdir)
    write(ieco,*) 'Input file : ',startdir(:len)//'\'//prefix(:l_prfx)//'.MUTbatch'
    write(ieco,*) 'Echo file  : ',startdir(:len)//'\'//prefix(:l_prfx)//'o.bat_eco'

	status = getcwd (workdir)

    skipping=.false.
    do
        read(ibat,10,iostat=status) instruction
        if(status /= 0) exit

        len=len_trim(instruction)

        10 format(a60)
        call lcase(instruction)
        if(skipping) then
            if(instruction .ne. skip_off) then
                write(ieco,'(a,a)') '       SKIP: ',instruction(:len)
                cycle
            else
                skipping=.false.
                write(ieco,'(a,a)') 'INSTRUCTION: ',instruction(:len)
                cycle
            endif
        endif

        write(ieco,'(a)') ' '
        write(ieco,'(a,a)') 'INSTRUCTION: ',instruction(:len)

        if(instruction .eq.  skip_on) then
            skipping=.true.

        else if(instruction .eq. pause_file) then
            pause 'MUTBatch processor paused, press any key to continue'

        else if(instruction .eq. do_runtime_history_cmd) then
            do_runtime_history=.true.
        
        else if(instruction .eq. mut_debug_cmd) then
            mut_debug=.true.
            
        elseif(instruction .eq. MUT_path_cmd) then
			read(ibat,'(a)',iostat=status) MUT_path
			len=len_trim(MUT_path)
            if(mut_debug) then
			    inquire(file=MUT_path(:len)//'mut_debug.exe',exist=file_exists)
			    if(.not. file_exists) then
				    call input_error(' File not found: '//MUT_path(:len)//'mut_debug.exe')
			    endif
			    write(ieco,'(a)') MUT_path(:len)//'mut_debug.exe'
            else
			    inquire(file=MUT_path(:len)//'mut.exe',exist=file_exists)
			    if(.not. file_exists) then
				    call input_error(' File not found: '//MUT_path(:len)//'mut.exe')
			    endif
			    write(ieco,'(a)') MUT_path(:len)//'mut.exe'
            endif

        elseif(instruction .eq. modflow_path_cmd) then
			read(ibat,'(a)',iostat=status) modflow_path
			len=len_trim(modflow_path)
			inquire(file=modflow_path(:len)//'usgs_1.exe',exist=file_exists)
			if(.not. file_exists) then
				call input_error(' File not found: '//modflow_path(:len)//'usgs_1.exe')
			endif
			write(ieco,'(a)') modflow_path(:len)//'usgs_1.exe'

        elseif(instruction .eq. MUTPost_path_cmd) then
			read(ibat,'(a)',iostat=status) MUTPost_path
			len=len_trim(MUTPost_path)
            if(mut_debug) then
			    inquire(file=MUTPost_path(:len)//'mut_debug.exe',exist=file_exists)
			    if(.not. file_exists) then
				    call input_error(' File not found: '//MUTPost_path(:len)//'mut_debug.exe')
			    endif
			    write(ieco,'(a)') MUTPost_path(:len)//'mut_debug.exe'
            else
			    inquire(file=MUTPost_path(:len)//'mut.exe',exist=file_exists)
			    if(.not. file_exists) then
				    call input_error(' File not found: '//MUTPost_path(:len)//'mut.exe')
			    endif
			    write(ieco,'(a)') MUTPost_path(:len)//'mut.exe'
            endif

        elseif(instruction .eq. change_dirs) then
			read(ibat,'(a)',iostat=status) instruction
			if(status /= 0) exit

	        len=len_trim(instruction)
			write(ieco,'(a)')instruction(:len)

			status = chdir(instruction)
			select case (status)
			case (enoent)
				call input_error('Directory does not exist')
			case (enotdir)
				call input_error('Not a directory')
			case default
				status = getcwd (workdir)
				write(ieco,'(a)') 'Directory changed'
			end select

        elseif(instruction .eq. process_dirs) then
		    
		    if(do_runtime_history .and. .not. scanning) then
		        call InitRuntimeHistory
            endif

			do
				read(ibat,'(a)',iostat=status) local_dir
				if(status /= 0) exit

				len=len_trim(local_dir)

				!call lcase(instruction)
				write(ieco,'(a,a)') '  DIR: ',local_dir(:len)

				if(local_dir(1:3) .eq. end_cmd) then
					call end_instruction('  DIR')
					exit
				else
					status = chdir(local_dir)
					if(create_runtime_file) then
					    write(uruntime,'(a)') 'zone t="'//local_dir(:len)//'"'
					endif
					select case (status)
					case (enoent)
						write(*,'(a)') 'Current working directory is '//workdir
						call input_error( '    The directory does not exist')
					case (enotdir)
						write(*,'(a)') 'Current working directory is '//workdir
						call input_error( '    Not a directory')
					case default
						write(ieco,'(a)') '    Directory successfully changed'
						if(scanning) then
						 !   inquire(file='batch.pfx',exist=batch_exists)
							!if(.not. batch_exists) call input_error('No batch.pfx file')
						else
							if(do_MUT) then
                                if(mut_debug) then
								    write(ieco,'(a,\)') '    Running MUT_debug _build...'
								    dummy=secnds(0.0)
								    len=len_trim(MUT_path)
								    i=system(MUT_path(:len)//'mut_degug.exe  _build')
                                else
								    write(ieco,'(a,\)') '    Running MUT _build...'
								    dummy=secnds(0.0)
								    len=len_trim(MUT_path)
								    i=system(MUT_path(:len)//'mut.exe  _build')
                                endif
								dummy=secnds(dummy)
								write(ieco,'(f15.5,a)') dummy,' seconds'
							end if
							if(do_Modflow) then
								write(ieco,'(a,\)') '    Running USGS_1 modflow'
								dummy=secnds(0.0)
								len=len_trim(modflow_path)
								i=system(modflow_path(:len)//'usgs_1.exe modflow')
								dummy=secnds(dummy)
								write(ieco,'(f15.5,a)') dummy,' seconds'
							end if
							if(do_MUTplot) then
                                if(mut_debug) then
								    write(ieco,'(a,\)') '    Running MUT_debug _post...'
								    dummy=secnds(0.0)
								    len=len_trim(MUTPost_path)
								    i=system(MUTPost_path(:len)//'mut_debug.exe _post')
                                else
								    write(ieco,'(a,\)') '    Running MUT _post...'
								    dummy=secnds(0.0)
								    len=len_trim(MUTPost_path)
								    i=system(MUTPost_path(:len)//'mut.exe _post')
                                endif
								dummy=secnds(dummy)
								write(ieco,'(f15.5,a)') dummy,' seconds'
							end if

							if(do_runtime_history) then
								inquire(file=lfile ,exist=file_exists)
								if(file_exists) then
								    open(57,file=lfile,status='unknown', form='formatted')
								    read(57,'(a)') line
								    close(57)
                								    
					                if(create_runtime_file) then
					                    write(uruntime,'(f12.5,1x,a)') dyear,line
					                else
					                    found=.false.
					                    do i=1,nrt
					                        if(local_dir(:len_trim(local_dir))==rt_name(i)) then ! append the data 
                                                ntimes(i)=ntimes(i)+1
                                                write(rt_data(i,ntimes(i)),'(f12.5,1x,a)') dyear,line(:len_trim(line))
                                                found=.true.
                                                exit
					                        endif
					                    end do
					                    if(.not. found) then ! create a new runtime zone
					                        nrt=nrt+1
					                        ntimes(nrt)=1
					                        rt_name(nrt)=local_dir(:len)
                                            write(rt_data(nrt,ntimes(nrt)),'(f12.5,1x,a)') dyear,line(:len_trim(line))
					                    endif
					                endif
								    
								endif
							end if

						endif
					end select

					status = chdir(workdir)
				endif
			end do
		    
		    if(do_runtime_history .and. .not. scanning .and. .not. create_runtime_file) then
		        call OverwriteRuntimeHistory
            endif

        else
			call input_error('MUTBatch PROCESSOR: Unrecognized instruction')
        endif

    end do
    
    

	if(scanning) then
		scanning=.false.
		status = chdir(startdir)
		rewind(56)
		ieco=66
		open(ieco,file=prefix(:l_prfx)//'o.bat_eco',status='unknown')
		goto 1000
    endif

	dummy2=secnds(dummy2)
	write(ieco,'(a,f15.5,a)') 'TOTAL Elapsed time MUTBatch: ',dummy2,' seconds'

end program MUTbatch_processor
!----------------------------------------------------------------------
subroutine InitRuntimeHistory
    use MUTbat_data
    implicit none
    
    character(4096) line
	logical :: file_exists
    character(128) ::rfile 
    integer :: l1

    ! runtime history file 
    rfile=startdir(:len_trim(startdir))//'\runtime_history.dat'
    inquire(file=rfile ,exist=file_exists)
    if(file_exists) then ! read it
        append_runtime_file=.true.
        nrt=0
        open(58,file=rfile,status='unknown', form='formatted')
        do
            read(58,'(a)',iostat=status) line
            if(status /= 0) then
                close(58)
                exit
            endif
            l1=index(line,'zone')
            if(l1/=0) then
                nrt=nrt+1
                rt_name(nrt)=line(l1+8:len_trim(line)-1)
                ntimes(nrt)=0
                do
                    read(58,'(a)',iostat=status) line
                    if(status /= 0) then
                        close(58)
                        exit
                    endif
                    l1=index(line,'zone')
                    if(l1/=0) then
                        backspace(58)
                        exit
                    endif
                
                    ntimes(nrt)=ntimes(nrt)+1
                    rt_data(nrt,ntimes(nrt))=line(:len_trim(line))
                end do

            endif    
            
        end do
    else  ! set flag to produce file and write header
        create_runtime_file=.true.
        uruntime=58
        open(uruntime,file=rfile,status='unknown', form='formatted')
        write(uruntime,*) '# runtime history file'
        write(uruntime,'(a)') 'variables="Year","total_sim_time","numthread","ntstep","NR_iteration","solver_iteration","nunk","wctime_assembly","wctime_iluc","wctime_solver"'
    endif


end subroutine InitRuntimeHistory

!----------------------------------------------------------------------
subroutine OverwriteRuntimeHistory
    use MUTbat_data
    implicit none
    
    character(128) ::rfile 
    integer :: j

    ! runtime history file 
    rfile=startdir(:len_trim(startdir))//'\runtime_history.dat'
    uruntime=58
    open(uruntime,file=rfile,status='unknown', form='formatted')
    write(uruntime,*) '# runtime history file'
    write(uruntime,'(a)') 'variables="Year","total_sim_time","numthread","ntstep","NR_iteration","solver_iteration","nunk","wctime_assembly","wctime_iluc","wctime_solver"'
    do i=1,nrt
	    write(uruntime,'(a)') 'zone t="'//rt_name(i)(:len_trim(rt_name(i)))//'"'
	    do j=1,ntimes(i)
	        write(uruntime,'(a)') rt_data(i,j)(:len_trim(rt_data(i,j)))
        end do
    end do
end subroutine OverwriteRuntimeHistory

!----------------------------------------------------------------------
subroutine enter_prefix(prefix,lp,nunit,ext)
    implicit none
    logical batch_exists,prepro_input_exists
    character*40 prefix
    character*(*) ext
    character*11 dext
    !rt-nov99
    character*40 fname
    integer l_ext,nunit,lp

    dext=ext
    l_ext=index(dext,' ')+1

    inquire(file='batch.pfx',exist=batch_exists)

    133 if(batch_exists) then
        open(nunit,file='batch.pfx',status='unknown')
        read(nunit,132,err=136) prefix
        goto 137

        136     write(*,*)'ERROR READING PREFIX IN BATCH.PFX '
        write(*,*)'Switching to interactive mode '
        write(*,*)'Try again...'
        batch_exists=.false.
        goto 133

        137     close(nunit)
    else
        WRITE(*,*) 'Enter a prefix for a .MUTBatch file: '
        read(*,132) prefix
        132     format(a32)
    endif

    lp=index(prefix,' ')-1
    if(lp.eq.-1 .or. lp.gt.40) then
        lp=40
    else if(lp.eq.0) then
        if(batch_exists) then ! bad batch.pfx file so switch to interactive
            write(*,*)'EMPTY PREFIX IN BATCH.PFX '
            write(*,*)'  Switching to interactive mode '
            write(*,*)'  Try again...'
            batch_exists=.false.
        else
            write(*,*) 'Empty prefix - try again'
        endif
        goto 133
    else
        fname =  prefix(:lp)//dext(:l_ext)
        !rt-nov99        inquire(file=prefix(:lp)//dext(:l_ext),exist=prepro_input_exists)
        inquire(file=fname,exist=prepro_input_exists)
        if(.not. prepro_input_exists) then
            if(batch_exists) then ! bad batch.pfx file so switch to interactive
                write(*,*)'COULD NOT FIND FILE: ',prefix(:lp)//dext(:l_ext)
                write(*,*)'  Switching to interactive mode '
                write(*,*)'  Try again...'
                batch_exists=.false.
            else
                write(*,*)'COULD NOT FIND FILE: ',prefix(:lp)//dext(:l_ext)
                write(*,*)'  Try again..'
            endif
            goto 133
        else
            !rt-nov99          open(nunit,file=prefix(:lp)//dext(:l_ext),
            open(nunit,file=fname,status='unknown',err=134)
            goto 135
            134         if(batch_exists) then ! bad batch.pfx file so switch to interactive
                write(*,*)'COULD NOT OPEN: ',prefix(:lp)//dext(:l_ext)
                write(*,*)'  Switching to interactive mode '
                write(*,*)'  Try again...'
                batch_exists=.false.
            else
                write(*,*)'COULD NOT OPEN: ',prefix(:lp)//dext(:l_ext)
                write(*,*)'  Try again...'
            endif
            goto 133
        endif
    endif

    135 continue

end subroutine enter_prefix
!----------------------------------------------------------------------
subroutine strip_comments(nin,nout)
    character*256 line, line_i
    logical :: file_exists
    integer :: status

    read_line: do
        read(nin,'(a)',iostat=status) line
        if(status /= 0) exit read_line

        line=adjustl(line)
        len=len_trim(line)

        if(line(1:1) == '!' .or. len == 0) then  ! a comment or blank line, do nothing

        else if(line(1:7) == 'include') then ! include a file
            inquire(file=line(9:),exist=file_exists)
            if(.not. file_exists) then
				call input_error('File not found: '//line(8:))
            else
				call getunit(itmp)
                open(itmp,file=line(9:),status='unknown',form='formatted')
                read_include_line: do
                    read(itmp,'(a)',iostat=status) line_i
                    if(status /= 0) exit read_include_line

                    len=len_trim(line_i)

                    if(line_i(1:1) == '!' .or. len == 0) then  ! a comment or blank line, do nothing

                    else ! instruction or data, write to nout
                        write(nout,'(a)') adjustl(line_i)
                    endif

                end do read_include_line
				call freeunit(itmp)
            end if


        else ! instruction or data, write to nout
            write(nout,'(a)') adjustl(line)
        endif

    end do read_line

    rewind(nout)

end subroutine strip_comments
!------------------------------------------------------------------------
subroutine lcase(string)
    character*40 :: string
    integer :: length,i,j
    length=len(string)
    do i=1,length
        j=ichar(string(i:i))
        if(j.ge.65 .and. j.le.90) then
            j=j+32
            string(i:i)=char(j)
        endif
    end do

end subroutine lcase
!----------------------------------------------------------------------
subroutine input_error(str)
	use MUTbat_data
	implicit none
	character(*) :: str

	write(ieco,'(a)') ' '
	write(ieco,'(a)') '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
	write(ieco,'(a)') '@@* INPUT ERROR, HALTING EXECUTION @@@@'
	write(ieco,'(a)') '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
	write(ieco,'(a)') ' '
	write(ieco,'(a)') str
	write(ieco,'(a)') ' '

	stop ! input error
end subroutine input_error
!----------------------------------------------------------------------
subroutine header(iunit)
	use version
    integer :: iunit, len

    write(iunit,*) '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
    write(iunit,*) '@@                                                                      @@'
	len=len_trim(local_name)
    write(iunit,'(1x,a)') local_name(:len)
    write(iunit,*) '@@                                                                      @@'
	!len=len_trim(local_version)
 !   write(iunit,'(1x,2a,T74,a)') '@@   REVISION:   ',local_version(:len),'@@'
	!len=len_trim(build_date)
 !   write(iunit,'(1x,2a,T74,a)') '@@   BUILD DATE: ',build_date(:len),'@@'
	!len=len_trim(build_specific_info)
 !   write(iunit,'(1x,2a,T74,a)') '@@   BUILD INFO: ',build_specific_info(:len),'@@'
 !
	!if(index(local_version,'M') > 0 .or. index(local_version,'S') > 0 .or. index(local_version,':') > 0) then
	!	len=len_trim(HGSDir)
	!	write(iunit,'(1x,2a,T74,a)') '@@   DIRECTORY:  ',HGSDir(:len),'@@'
	!	write(iunit,*) '@@                                                                       @@'
	!	write(iunit,*) '@@   REPOSITORY INFORMATION:                                             @@'
	!	len=len_trim(repo_1)
	!	write(iunit,'(1x,2a,T74,a)') '@@   ',repo_1(:len),'@@'
	!	len=len_trim(repo_2)
	!	write(iunit,'(1x,2a,T74,a)') '@@   ',repo_2(:len),'@@'
	!	len=len_trim(repo_3)
	!	write(iunit,'(1x,2a,T74,a)') '@@   ',repo_3(:len),'@@'
	!	len=len_trim(repo_4)
	!	write(iunit,'(1x,2a,T74,a)') '@@   ',repo_4(:len),'@@'
	!	len=len_trim(repo_5)
	!	write(iunit,'(1x,2a,T74,a)') '@@   ',repo_5(:len),'@@'
	!	len=len_trim(repo_6)
	!	write(iunit,'(1x,2a,T74,a)') '@@   ',repo_6(:len),'@@'
	!	len=len_trim(repo_7)
	!	write(iunit,'(1x,2a,T74,a)') '@@   ',repo_7(:len),'@@'
	!	len=len_trim(repo_8)
	!	write(iunit,'(1x,2a,T74,a)') '@@   ',repo_8(:len),'@@'
	!	len=len_trim(repo_9)
	!	write(iunit,'(1x,2a,T74,a)') '@@   ',repo_9(:len),'@@'
	!	write(iunit,*) '@@                                                                      @@'
	!endif
    !write(iunit,*) '@@                                                                      @@'
    !write(iunit,*) '@@   (c) Rene Therrien, 1993 - 2006                                     @@'
    !write(iunit,*) '@@       Rob McLaren                                                    @@'
    !write(iunit,*) '@@       E. A. Sudicky                                                  @@'
    write(iunit,*) '@@                                                                      @@'
    write(iunit,*) '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'

	write(iunit,*) ' '
end subroutine header

!----------------------------------------------------------------------
function decyear(datestring,timestring)
	use machine_constants
	implicit none

	real(dr) :: decyear

	character*(*) :: datestring, timestring
	real(dr) :: year, month, day, hour, minute, second, cumul_days(11), tot_days
	data cumul_days/31.,59.,90.,120.,151.,181.,212.,243.,273.,304.,334./

	read(datestring(1:4),*) year
	read(datestring(5:6),*) month
	read(datestring(7:8),*) day

	read(timestring(1:2),*) hour
	read(timestring(4:5),*) minute
	second=0.0
	if(len_trim(timestring)==8) read(timestring(7:8),*) second

	tot_days=0.0
!tr 2007.05.23	if(month>1) tot_days=tot_days+cumul_days(month-1)
	if(month>1) tot_days=tot_days+cumul_days(int(month)-1)
	tot_days=tot_days+day
	tot_days=tot_days+hour/24.
	tot_days=tot_days+minute/1440.
	tot_days=tot_days+second/86400.

	decyear=year+tot_days/365.0

end function decyear
