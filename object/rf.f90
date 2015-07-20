! Subfile.f
!c
!c********
!c open unformatted, direct access, old file
! udo ("f"ormatted, "d"irect access, "o"ld)
!
! subroutine of_udo (un,bs)
! subroutine of_udo_filen (file,un,bs)
! subroutine of_udr (un,bs)
! subroutine of_fdr (un)
! subroutine of_fdo (un,bs, ed)
! subroutine of_udu (un,bs, ed)
! 
subroutine of_udo (un,bs, le)

  character(150) file
  character(20)  ed
  integer un,bs, le ! unit number and block size

  if(le.eq.1)then
     ed="little_endian"
  elseif(le.eq.0)then
     ed="big_endian"
  endif

  read(5,'(A150)') file
  write(6,'(A150)') file
  open(unit=un,status='old',file=file,FORM='UNFORMATTED', &
  & recl=bs,access='direct',err=100, convert=ed) 

  write(6,*) " has opened successfully.", "File No", un,ed
  return

100 write(6,*) " has error in opening.", "File No", un,ed
  stop
end subroutine of_udo

!********
! open unformatted, direct access, old file with file name
!udo ("f"ormatted, "d"irect access, "o"ld)
subroutine of_udo_filen (file,un,bs)

  character(150) file
  integer un,bs ! unit number and block size

  read(5,'(A150)') file
  open(unit=un,status='old',file=file,FORM='UNFORMATTED', &
  & recl=bs,access='direct',err=100)
  write(6,'(A150)') file
  write(6,*) " has opened successfully."
  return

100 write(6,'(A150)') file
  write(6,*) " has error in opening."
  stop
end subroutine of_udo_filen


! open unformatted, direct access, replace (unknown) file
! udr ("u"nformatted, "d"irect access, "r"eplace)
subroutine of_udr (un,bs)
  
  character(150) file
  integer un,bs ! unit number and block size
  
  read(5,'(A150)') file
  open(unit=un,status='replace',file=file, &
       & FORM='UNFORMATTED', &
       & recl=bs,access='direct',err=100)
  write(6,'(A150)') file
  write(6,*) "has opened successfully."
  return

100 write(6,'(A150)') file
  write(6,*) " has error in opening."
  stop
end subroutine of_udr

! -------------------------------------------------------
!! open formatted, replace (unknown) file
! fdr ("f"ormatted, "d"irect access, "r"eplace)
subroutine of_fdr (un)

  character(150) file
  integer un ! unit number and block size

  read(5,'(A150)') file
  open(unit=un,status='replace',file=file, &
       & FORM='FORMATTED',err=100)

  write(6,'(A150)') file
  write(6,*) "has opened successfully."
  return

100 write(6,'(A150)') file
  write(6,*) " has error in opening."
  stop
end subroutine of_fdr

! ----------------------------------------
!! open formatted, old file
! fdu ("f"ormatted, "d"irect access, "u"nknown)
subroutine of_fdo (un)

  character(150) file
  integer un ! unit number and block size

  read(*,'(A150)') file
  open(unit=un,status='old',file=file, &
       & FORM='FORMATTED',err=100)

  write(6,'(A150)') file
  write(6,*) "has opened successfully."
  return

100 write(6,'(A150)') file
  write(6,*) " has error in opening."
  stop
end subroutine of_fdo


!******** 
! open unformatted, direct access, replace (unknown) file
! udu ("u"nformatted, "d"irect access, "u"nknown)
subroutine of_udu (un, bs, le)

  character(200) file
  character(30) ed
  integer un,bs,le ! unit number and block size, endian
  
  if(le.eq.1)then
     ed="little_endian"
  elseif(le.eq.0)then
     ed="big_endian"
  endif

  read(*,'(A200)') file
  open(unit=un,file=file, &
       &     FORM='UNFORMATTED', &
       &     recl=bs, access='direct',convert=ed, err=100)
!       &     FORM='UNFORMATTED', &
!status='unknown', &

  write(6,'(A200)') file
  write(6,*) "has opened successfully.", "File No.",un, bs, le
  return

100 write(6,'(A200)') trim(file)
  write(6,*) " has error in opening.", "File No.",un, bs, le
  stop
end subroutine of_udu


! read file of multi level data
subroutine read_data (var, fn, irecn, yrev, ierr, defx,defy)
  implicit none
  integer fn,irecn, yrev, ix, iy, fnum, ierr
  integer defx, defy
  real(4) var(defx,defy)

  if(yrev==1)then
     read(fn,rec=irecn) ((var(ix,iy),ix=1,defx),iy=defy,1,-1)
  elseif(yrev==0)then
     read(fn,rec=irecn) var
  endif

return
end subroutine read_data

!
! read file of multi level data
subroutine readf_mlev (defx,defy, irecn, yrev,var,fnum)
  implicit none
  integer defx,defy,irecn, yrev, ix, iy, fnum
  real(4) var(defx,defy)

  if(yrev==1)then
     read(10,rec=irecn) ((var(ix,iy),ix=1,defx),iy=defy,1,-1)
  elseif(yrev==0)then
     read(10,rec=irecn) var
  endif
return
end subroutine readf_mlev

!read data of single level
subroutine readf_slev(defx,defy,irecn,yrev,var,fnum)
  implicit none
  integer  defx,defy,irecn, yrev, ix,iy,fnum
  real(4) var(defx,defy)
  if(yrev==1)then
     read(10,rec=irecn) ((var(ix,iy),ix=1,defx),iy=defy,1,-1)
  elseif(yrev==0)then
     read(10,rec=irecn) var
  endif
  return
end subroutine readf_slev


!*******************************************************

integer function ncepr1_rnum  (iyear, imo, ierr,mlev, nlev)
!  &(iyyyy,imm,idd,ihh,lyyyy,lmm,ldd,lhh,datanumdaily)

  
  integer iyear,imo,recn
  integer mlev, it, ierr
  integer, optional:: nlev
  
! function
!  integer validdate,comparedateorder,diffday
  integer, parameter :: defz=17, syear=1948
  ierr=0
  
  it=iyear-syear+1

  if(mlev==1)then
     ncepr1_rnum=(defz*((12*(it-1))+imo-1))+nlev
  elseif(mlev==0)then
     ncepr1_rnum=(12*(it-1))+imo
  endif


!  if(rcepr1_rnum .lt. 0)then
!     write(*,*) in, ncepr1_rnum, "-------------------"
!     stop
!     ncepr1_rnum=-999
!     return
!  endif

  return
end function ncepr1_rnum

integer function mo_rnum  (iyear, imo, ierr, mlev, nlev, syr, defz, smo,lag)

  integer iyear,imo, ierr, mlev, nlev,  syr, defz, smo
  integer recn, it
!  integer, optional:: nlev
  integer, optional :: lag

  ierr=0

  it=iyear-syr+1
!  write(*,*) (12*(it-1))+imo-smo+1
  if(mlev==1)then
     mo_rnum=(defz*(12*(it-1)+imo-smo+lag))+nlev
  elseif(mlev==0)then
     mo_rnum=(12*(it-1))+imo-smo+1+lag
  endif

  return
end function mo_rnum
