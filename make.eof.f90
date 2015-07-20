program eof
  implicit none
  character :: jobz,uplo
  integer :: info

  integer,parameter :: &
       & defyr=63

  real,parameter :: &
       & undef_old=999,a=6.37E6

  real(4), allocatable :: &
       & var(:,:,:,:), &
       & vara(:,:,:,:), &
       & clim(:,:,:),&
       & std(:,:,:), &
       & mon (:,:), &
       & momo(:,:,:), &
       & emat(:,:), &
       & cov(:,:), &
       & uall(:,:), &
       & vall(:,:), &
       & ev(:), &
       & evxy(:,:), &
       & eV2(:), &
       & TS(:), &
       & w(:), work (:)

  real (4) ::  undef, lev, pi, dx, dy, &
       & r,sum_nume, sum_deno,&
       & ave, sgm, avez2,sum2, temp, &
       & lon1, lon2, lat1, lat2, &
       & rmsPC

  character(150) :: FileName1, FileName2

  integer :: ix,iy,iz,it, iyr, lwork, pc, &
       & irec, mo_rnum, iyear,&
       & k, j, n,  &
       & num, calmo, imo,  &
       & defx, defy, defz, deft, &
       & defx2, defy2, defk, &
       & syr, eyr, dyr, syear, &
       & nlev, surface, &
       & le, bs, recn, &
       & bs2, &
       & yrev,mlev, smo,emo, &
       & ierr, readcheck, &
       & x1, x2, y1, y2, &
       & mocom1, mocom2, mocom3

  real :: var2,ave2

  real, allocatable :: var1(:),ave1(:)
  real, allocatable  :: cor(:),cor2(:,:), reg(:), regxy(:,:), cov12(:)

!
!  !!!!!!!!!  intialization  !!!!!!!!!!!!!!!!!!!!!!!!
!
  read(*,*) undef;   write(*,*) undef
  read(*,*) defx  ;   write(*,*) 'defx',defx
  read(*,*) defy;   write(*,*) 'defy',defy
  read(*,*) defz;   write(*,*) 'defz',defz
  read(*,*) defx2;   write(*,*) 'defx2',defx2
  read(*,*) defy2;   write(*,*) 'defy2',defy2
  read(*,*) lon1; read(*,*) lon2
  read(*,*) lat1; read(*,*) lat2; write(*,*) 'domain of EOF',lon1, lon2, lat1, lat2
  read(*,*) dx  ;   write(*,*) 'dx',dx
  read(*,*) dy;   write(*,*) 'dy',dy
  read(*,*) syr;   write(*,*) 'start year',syr
  read(*,*) syear;   write(*,*) 'start year of Re analysis data',syear
  read(*,*) eyr;   write(*,*) 'end year',eyr
  read(*,*) dyr;   write(*,*) 'duration',dyr
!  read(*,*) lev;   write(*,*) 'lev', lev
  read(*,*) nlev;   write(*,*) 'nlevber e.g. z=1000 nlev=1',nlev
  read(*,*) surface;   write(*,*) 'surface',surface
  read(*,*) calmo;   write(*,*) 'calculated month', calmo
  read(*,*) mocom1; read(*,*) mocom2
  read(*,*) mocom3; write(*,*) 'month', mocom1, mocom2, mocom3
  read(*,*) le;   write(*,*) 'little endian of inputdata?', le
  read(*,*) smo;   write(*,*) 'month of the first input data?', smo
  read(*,*) mlev;   write(*,*) 'Multi level data?', mlev
  read(*,*) yrev;   write(*,*) 'Is input data reverse?', yrev

  if( surface ==1)then
     defz=1
  endif

  lwork=defx*defy
  deft=dyr
  if(calmo>=13)deft=dyr*3
  defk=defx2*defy2

  x1= 1+int(lon1/dx,4)
  x2= 1+int(lon2/dx,4)
  y1= 1+int((lat1+90)/dy,4)
  y2= 1+int((lat2+90)/dy,4)
  write(*,*) "domain", x1, x2, y1, y2
  lwork=defk*10

  allocate ( &
       & clim    (defx,defy,14), &
       & std    (defx,defy,14), &
       & mon (defx,defy) , &
       & emat (defk,defk) , &
       & cov (defk,defk) , &
       & uall (defk,deft) , &
       & w(defk) , &
       & TS(deft), &
       & eV2(defk) , &
       & evxy(defx2,defy2) , &
       & var (defx,defy,dyr, 12) , &
       & vara (defx,defy,dyr, 12) , &
       & momo(defx,defy,14), &
       & var1(defk), ave1(defk), &
       & cor(defk), cor2(defx2, defy2), &
       & reg(defk), cov12(defk), work(lwork), &
       & regxy(defx2, defy2) )

  var=undef
  pi=acos(-1.d0)
!
! !!!!!!!!1111    I/O file data     !!!!!!!!!!!!!!!!!!!!11
!
  bs=defx*defy*4
  call of_udo(10, bs, le) !input
  bs=defx2*defy2*4
  bs2=deft*4
  call of_udu(100, bs, le) ! PC1 Eigen Vectour
  call of_udu(110, 4, le) ! PC1
  call of_udu(120, bs, le) ! corr (PC1, matrix of anomalies) in PC
  call of_udu(130, bs, le) ! reg (PC1, matrix of anomalies) in PC

  if(calmo==13)then
     call of_udu(140, 4, le) ! (PC1, matrix of anomalies) in PC
  endif
  call of_fdr(200) ! (Domain of each PCs)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  clim(:,:,:)=0.d0
!  landmask(:,:)=0.d0

  emo=smo
  if(smo<4)then
     emo=4
  endif

  readcheck=1.d0
  do imo=1,12
     do iyr=syr,eyr
        if(syr==iyr .and. emo > imo)cycle
        if(iyr==eyr .and. emo <= imo)cycle
        irec=mo_rnum(iyr,imo,ierr,mlev,nlev,syear,defz,smo, 0)
!        write(*,*) iyr,imo,syr,irec
        call read_data (mon, 10, irec, yrev, ierr, defx, defy, 0)

        it=iyr-(syr-1)
        if(imo<emo)it=iyr-syr
        var(:,:,it,imo)=mon(:,:)
!              readcheck=0.d0

        write(*,*) "iyear", iyr, "it=", it, "imo==",imo, irec
     enddo
  enddo

  do imo=1,12
     do it=1,dyr
        clim(:,:,imo)=var(:,:,it,imo)/(dyr)+clim(:,:,imo)
        momo(:,:,imo)=var(:,:,it,imo)*var(:,:,it,imo)+momo(:,:,imo)
     enddo
  enddo

  std(:,:,:)=(momo(:,:,:)/(real(dyr,4)-1.d0))-clim(:,:,:)

  write(*,*) "******************"
  write(*,*) "calculated average"
  write(*,*) "******************"

  ! DJF climatology
  clim(:,:,13)=(clim(:,:,12) + clim(:,:,1)+ clim(:,:,2))/3.
  clim(:,:,14)=(clim(:,:,11) + clim(:,:,12) + clim(:,:,1) &
       & + clim(:,:,2) + clim(:,:,3) )/5.d0

!
! !!!!!!!!!!!!   Confirm Undef value of climatology data  !!!!!!!!!!!!!!!!!!!!
!

!  if(smo==13)then
  do ix=1, defx
     do iy=1, defy
        if(clim(ix,iy,12)<-1E+10 .or. &
             & clim(ix,iy,1)<-1E+10 .or. &
             & clim(ix,iy,2)<-1E+10)then
           clim(ix,iy,13)=undef
        endif
     enddo
  enddo
!  endif


  ! DJF  & NDJFM  Standard deviation
  std(:,:,13)=( std(:,:,12) + &
       & std(:,:,1)+ std(:,:,2) ) * &
       & (dyr-1.d0) / (dyr*3.d0 - 1.d0)
  std(:,:,14)= ( std(:,:,11)  +  std(:,:,12)  +  std(:,:,1)  + &
       &  std(:,:,2)  +   std(:,:,3) ) &
       & * (dyr -1.d0) / ((dyr*5.d0) -1.d0)

  std(:,:,:)=sqrt(std(:,:,:))

  write(*,*) "******************"
  write(*,*) "calculated climatology & std"
  write(*,*) "******************"
  write(*,*) 'start year',syr,'end year',eyr, dyr

  pi=acos(-1.)

  do iyr=syr,eyr
     do imo=1, 12
        it=iyr-(syr-1)
        if(imo<emo) it=iyr-syr

        if(syr==iyr .and. emo > imo)cycle
        if(iyr==eyr .and. emo <= imo)cycle

        vara(:,:,it,imo) = var(:,:,it,imo) -clim(:,:,imo)
     enddo
  enddo

! var(:,:,:) to var(:,:)
  k=0;it=0; n=0
  do it=1,dyr
     do imo=1, 12
        if(imo==mocom1 .or. imo==mocom2 .or. imo==mocom3)then
           n=n+1
           do ix=x1, x2
              do iy=y1, y2
                 k=k+1
                 uall(k, n) = vara(ix,iy,it,imo) &
                      & * sqrt(abs(cos(( (iy-1)*dy-90.d0)*pi/180.d0)))
              enddo
           enddo
           k=0 ! ; write(*,*) n
        endif
     enddo
  enddo

  write(*,*) "cal vara"

  cov(:,:)=(matmul(uall, transpose(uall)) )!/(deft-1)
  emat=cov

  jobz = 'V'
  uplo = 'U'
  write(*,*) "cal covariance"

  call SSYEV( jobz, uplo, defk, emat, defk, w, work, lwork, info)

  write(*,*) "info (whether success or error on cal EV.)",  info
  do pc=1, 3
     TS(:)=matmul(emat(:, defk-pc+1), uall)

     ave = 0.; rmsPC=0.d0
     do iyear = 1, deft
        ave = ave + TS(iyear)
        rmsPC = rmsPC + (TS(iyear) ** 2)
     enddo
     ave=ave/real(deft)
     rmsPC = sqrt(rmsPC)/real(deft)

     sgm = 0.
     do iyear = 1, deft
        sgm = sgm + (TS(iyear)-ave)*(TS(iyear)-ave)
     enddo
     sgm=sqrt(sgm/real(deft))
     TS(:)=(TS(:)-ave)/sgm

     ave1(:) = 0.;  ave2 = 0.; var1(:) = 0.; var2 = 0.
     do iyear = 1, deft
        ave1(:) = ave1(:) + uall(:, iyear)
        ave2 = ave2 + TS(iyear)
     enddo

     do iyear = 1, deft
        var1(:) = var1(:) + (uall(:, iyear))*(uall(:,iyear))
        var2 = var2 + (TS(iyear)-ave2)*(TS(iyear)-ave2)
        cov12(:)=cov12(:)+ (TS(iyear)-ave2)*(uall(:,iyear)-ave1(:))
     enddo
     var1(:)=var1(:)/real(deft)
     var2=var2/real(deft)
     cov12(:)=cov12(:)/real(deft)

     cor(:)=cov12(:)/sqrt(var1(:)*var2)
     reg(:)=cov12(:)/var2

     k=0;it=0; imo=12
     do ix=1, defx2
        do iy=1, defy2
           k=k+1
           cor2(ix,iy) = cor(k)
           regxy(ix,iy) = reg(k)
           evxy(ix,iy) = emat(k, defk-pc+1)
        enddo
     enddo

     write(*,*) 'EOF domain ','PC=', pc, 100*w(defk-pc+1)/sum(w(:),dim=1)
     write(*,*) "RMS of PC",pc,rmsPC
     write(200,*) 'EOF domain ','PC=', pc, 100*w(defk-pc+1)/sum(w(:),dim=1)
     write(200,*) "RMS of PC",pc,rmsPC
     write(100, rec=pc) evxy(:,:)
     write(120, rec=pc) cor2(:,:)
     write(130, rec=pc) regxy(:,:)

     do it=1, deft
        if(calmo==13)then
           if(mod(it,3) /= 1)then
              temp=TS(it-1)
           elseif(mod(it,3) == 1)then
              temp=TS(it+2)
           endif
        else
           temp=TS(it)
        endif
           write(110, rec=pc+(it-1)*3) temp !TS(it)
     enddo

!NOTICE!!: it = (it=1, 1), (it=1, 2), (it=1, 12), (it=2, 1)
! imo /it 1       2     3 ・・・
! 1       undef  index(1,1)
! 2       undef  index(1,1)
! ・
! 12       index(1,12) undef   ・・・ index(dyr, 12) undef
!
     if(calmo==13)then !rewrite for pc = 1 ~ 3 not only pc =1
        k=0
        do it=1, dyr+1
           do imo=1,12
              recn=(((it-1)*12+imo-1)*3)+pc
              if(imo<3 .and. it==1)then !it=1 and imo<3 undef
                 write(140, rec=recn) undef
              elseif(imo>3 .and. it==(dyr+1))then !it=dyr and imo>3 undef
                 write(140, rec=recn) undef
              else
                 if(imo<3 .or. imo==12)then
                    k=k+1
                    temp=TS(k)
                    if(imo==12)temp=TS(k+2)
                    if(imo<3)temp=TS(k-1)
                 else
                    temp=undef
                 endif
                 write(140, rec=recn) temp
              endif
           enddo
        enddo
     endif
  enddo
 write(*,*) 'dot_product',dot_product(emat(:,defk),emat(:,defk-1))
 write(200,*) 'dot_product',dot_product(emat(:,defk),emat(:,defk-1))

 close(10)
 close(100)
 close(110)
 close(120)
 close(130)

  stop


end program eof
