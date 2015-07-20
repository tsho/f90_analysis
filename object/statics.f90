subroutine weltchtest(pos_ano, neg_ano, pos_std, neg_std, pn_t, pn_tw, freenum, tw99, defx, defy, num_p, num_n)
   
  integer num_p, num_n, defx,defy
  real(4) pos_ano(defx,defy),  neg_ano(defx,defy)
  real(4) pos_std(defx,defy),  neg_std(defx,defy)
  real(4) pn_t(defx,defy),  pn_std(defx,defy), pn_tw(defx,defy)
  real(4) freenum(defx,defy),  tw99(defx,defy)
  
  real(4) t90(12),t95(12),t99(12)

  data (t90(k),k=1,12) &
       &/6.31, 2.92, 2.35, 2.13, 2.02, &
       & 1.94, 1.89, 1.86, 1.83, 1.81, &
       & 1.80, 178 /

  data (t95(k),k=1,12) &
       &/12.71, 4.30, 3.18, 2.78, 2.57, &
       &2.45, 2.36, 2.31, 2.26, 2.23, &
       & 2.20, 2.18/

  data (t99(k),k=1,12) &
       &/63.66, 9.93, 5.84, 4.60, 4.03, &
       & 3.71, 3.50, 3.36, 3.25, 3.17, &
       & 3.11, 3.06 /

  do ix=1,defx
     do iy=1,defy
!        if(clim(ix,iy,imo2)/=undef)then
           pn_std(ix,iy)=&
                & ((real(num_p,4)-1.d0)*pos_std(ix,iy)+ &
                & ((real(num_n,4)-1.d0)*neg_std(ix,iy)))/ &
                & (real(num_p,4)+real(num_n,4)-2.d0)
           
           pn_t(ix,iy)= &
                & ( pos_ano(ix,iy)-neg_ano(ix,iy) )/ &
                & (pn_std(ix,iy) * &
                & sqrt((1.d0/real(num_p,4))+(1.d0/real(num_n,4)  )))

           pn_tW(ix,iy)=-1.d0*(pos_ano(ix,iy)-neg_ano(ix,iy))/&
                & sqrt((pos_std(ix,iy)/num_p)+ (neg_std(ix,iy)/num_n))

           freenum(ix,iy) = &
                & (( (pos_std(ix,iy)/real(num_p,4)) + &
                & (neg_std(ix,iy)/real(num_n,4)) )**2) / &
                & ((((pos_std(ix,iy)/real(num_p,4))**2)/(real(num_p,4)-1.d0)) + &
                & (((neg_std(ix,iy)/real(num_n,4))**2)/(real(num_n,4)-1.d0))   ) 
!        endif

        freenum(ix,iy)=nint(freenum(ix,iy))

        if (t90(int(freenum(ix,iy))) < (pn_tw(ix,iy)))then
           tw99(ix,iy)=90.d0
           if (t95(int(freenum(ix,iy))) < (pn_tw(ix,iy)))then
              tw99(ix,iy)=95.d0
              if (t99(int(freenum(ix,iy))) < (pn_tw(ix,iy)))then
                 tw99(ix,iy)=99.d0
              endif
           endif
        elseif(-1.d0*t90(int(freenum(ix,iy))) > (pn_tw(ix,iy)))then
           tw99(ix,iy)=-90.d0
           if(-1.d0*t95(int(freenum(ix,iy))) > (pn_tw(ix,iy)))then
              tw99(ix,iy)=-95.d0
              if(-1.d0*t99(int(freenum(ix,iy))) > (pn_tw(ix,iy)))then
                 tw99(ix,iy)=-99.d0
              endif
           endif
        else
           tw99(ix,iy)=undef
        endif
     enddo
  enddo

  return

endsubroutine weltchtest


subroutine teleconnectivity(defx,defy,defz,undef,syr,eyr,dyr,var,clim, calmo,  nlev, smo, mlev, yrev,tc, lontc, lattc, flg)
  implicit none
  integer :: ix,iy,it, iyr,&
       & k, j, n, &
       & defx, defy, defz,&
       & syr, eyr, dyr,&
       & imo, calmo, &
       & smo, mlev, nlev, yrev, &
       & irec, ierr, &
       & readcheck, &
       & sumcheck, &
       & mo_rnum, &
       & sit, eit

  integer(4) :: &
       & flg(defx, defy), &
       & fnum(defx,defy)

  real(4) :: &
       & clim(defx,defy, 14), &
       & sum1(defx,defy), &
       & sum2(defx,defy), &
       & corr(defx,defy), &
       & tc(defx,defy), lattc(defx,defy), lontc(defx,defy), &
       & mon(defx,defy), &
       & loc(2), &
       & var(defx,defy,dyr,12), &
       & cov(defx,defy)

  real :: temp,lat,lon, &
       & dx, dy, &
       & lonx, laty, &
       & undef

  character (150) :: FileName1, FileName2

!  write(*,*) 'File No.=10 ',FileName1 
  write(*,*) "defx",defx,"defy",defy  
  write(*,*) 'defz',defz,'nlev',nlev,defx,defy

!  write(100,rec=nlev) ((clim(ix,iy,1),ix=1,defx),iy=1,defy)
!stop

!------------------------------------------
! main program
!------------------------------------------
  dx=360.d0/real(defx,4)
  dy=180.d0/real(defy,4)

  if(mod(defy,2)/=0.d0)dy=180/(real(defy-1.d0,4))

  sum1=0.d0;  corr=0.d0;  sum2=0.d0
  readcheck=0.d0
  sumcheck=0.d0
  k=1;j=1

!     write(*,*) 'k',k, 'j',j
!  do k=310,310
  do j=1,defy
     write(*,*) 'k',k, 'j',j
     do k=1,defx

        temp=0.0
        n=0
        do it=1,dyr
           do imo=1,12

              if(calmo==14)then
                 if (imo==11 .or. imo==12 .or. imo==1 .or. imo==2 .or. imo==3)then
                    readcheck=1.d0
                 endif
              elseif(calmo==13)then
                 if (imo==12 .or. imo==1 .or. imo==2)then
                    readcheck=1.d0
                 endif
              else
                 if(calmo==imo)then
                    readcheck=1.d0
                 endif
              endif

              if(readcheck==1.d0)then
                 n = n + 1
                 do ix=1, defx
                    do iy=1, defy
                       if(flg(k,j) < 10 .or. flg(ix,iy) < 10)then
!                          fnum(ix,iy) = 1.d0 + fnum(ix,iy)
                          cov(ix,iy)=cov(ix,iy)+&
                               & ((var(ix,iy,it,imo)-clim(ix,iy,imo))* & 
                               & (var(k,j,it,imo)-clim(k,j,imo)))
                          if(sumcheck/=1.d0)then
                             sum1(ix,iy)=sum1(ix,iy)+(var(ix,iy,it,imo)-clim(ix,iy,imo))**2
                          endif

                       endif
                    enddo
                 enddo
                 readcheck=0.d0
              endif
           enddo !imo
        enddo !it

!        corr(:,:)=(corr(:,:))/(sqrt(sum1(:,:))*sqrt(sum1(k,j)))
!        cov(:,:)=cov(:,:)/real(n-1)
!        cov(:,:)=cov(:,:)

        if(sumcheck/=1.d0)then
!           sum1(:,:)=sum1(:,:)/real(n-1)
           sumcheck=1.d0
        endif
        corr(:,:)=(cov(:,:))/(sqrt(sum1(:,:))*sqrt(sum1(k,j)))

!        write(*,*) n-1

        if(sumcheck/=1.d0)then
           sumcheck=1.d0
        endif
        temp=minval(corr)
        loc=minloc(corr)
        lon=(real(loc(1),4)-1)*dx
        lat=(real(loc(2),4)-1)*dy-90.d0

        if(temp > 1.d0 .or. temp < -2.)then
           write(*,*) '*****************************************************'
           write(*,*) "Warning teleconnectivity dosen't have a negative value"
           write(*,*) '*****************************************************'
           write(*,*) 'corr',temp,k,j, loc(1),loc(2)
           temp=undef
        endif

        tc(k,j)=temp
        lattc(k,j)=lat
        lontc(k,j)=lon

        if(flg(k,j) >= 10)then
           tc(k,j)=undef
           lattc(k,j)=undef
           lontc(k,j)=undef
        endif

        temp=0.d0; corr=0.d0; cov=0.d0
!        return
     enddo
  enddo


  write(*,*) '*****************************'
  write(*,*) 'wrote teleconnectivity'
  write(*,*) '*****************************'

  return
  stop
end subroutine teleconnectivity

subroutine teleconnectivity_noundef(defx,defy,defz,undef,syr,eyr,dyr,var,clim, calmo,  nlev, smo, mlev, yrev,tc, lontc, lattc)
  implicit none
  integer :: ix,iy,it, iyr,&
       & k, j, n, &
       & defx, defy, defz,&
       & syr, eyr, dyr,&
       & imo, calmo, &
       & smo, mlev, nlev, yrev, &
       & irec, ierr, &
       & readcheck, &
       & sumcheck, &
       & mo_rnum, &
       & sit, eit

  integer(4) :: &
       & flg(defx, defy)

  real(4) :: &
       & clim(defx,defy, 14), &
       & sum1(defx,defy), &
       & sum2(defx,defy), &
       & corr(defx,defy), &
       & tc(defx,defy), lattc(defx,defy), lontc(defx,defy), &
       & mon(defx,defy), &
       & loc(2), &
       & var(defx,defy,dyr,12), &
       & cov(defx,defy)

  real :: temp,lat,lon, &
       & dx, dy, &
       & lonx, laty, &
       & undef

  character (150) :: FileName1, FileName2

!  write(*,*) 'File No.=10 ',FileName1 
  write(*,*) "defx",defx,"defy",defy  
  write(*,*) 'defz',defz,'nlev',nlev,defx,defy

!  write(100,rec=nlev) ((clim(ix,iy,1),ix=1,defx),iy=1,defy)
!stop

!------------------------------------------
! main program
!------------------------------------------
  dx=360.d0/real(defx,4)
  dy=180.d0/real(defy,4)

  if(mod(defy,2)/=0.d0)dy=180/(real(defy-1.d0,4))

  sum1=0.d0;  corr=0.d0;  sum2=0.d0
  readcheck=0.d0
  sumcheck=0.d0
  k=1;j=1

!     write(*,*) 'k',k, 'j',j
!  do k=310,310
  do j=1,defy
     write(*,*) 'k',k, 'j',j
     do k=1,defx

        temp=0.0
        n=0
        do it=1,dyr
           do imo=1,12

              if(calmo==14)then
                 if (imo==11 .or. imo==12 .or. imo==1 .or. imo==2 .or. imo==3)then
                    readcheck=1.d0
                 endif
              elseif(calmo==13)then
                 if (imo==12 .or. imo==1 .or. imo==2)then
                    readcheck=1.d0
                 endif
              else
                 if(calmo==imo)then
                    readcheck=1.d0
                 endif
              endif

              if(readcheck==1.d0)then
                 n = n + 1

                 if(sumcheck/=1.d0)then
                    sum1(:,:)=sum1(:,:)+(var(:,:,it,imo)-clim(:,:,imo))**2
                 endif
                 cov(:,:)=cov(:,:)+&
                      & (var(:,:,it,imo)-clim(:,:,imo))*&
                      & (var(k,j,it,imo)-clim(k,j,imo))
                 readcheck=0.d0
              endif
           enddo !imo
        enddo !it

        corr(:,:)=(cov(:,:))/(sqrt(sum1(:,:))*sqrt(sum1(k,j)))

!        cov(:,:)=cov(:,:)/real(n-1)
        if(sumcheck/=1.d0)then
!           sum1(:,:)=sum1(:,:)/real(n-1)
           sumcheck=1.d0
        endif


        if(sumcheck/=1.d0)then
           sumcheck=1.d0
        endif
        temp=minval(corr)
        loc=minloc(corr)
        lon=(real(loc(1),4)-1)*dx
        lat=(real(loc(2),4)-1)*dy-90.d0

        if(temp > 1.d0 .or. temp < -2.)then
           write(*,*) '*****************************************************'
           write(*,*) "Warning teleconnectivity dosen't have a negative value"
           write(*,*) '*****************************************************'
           write(*,*) 'corr',temp,k,j, loc(1),loc(2)
           temp=undef
        endif

        tc(k,j)=temp
        lattc(k,j)=lat
        lontc(k,j)=lon
        temp=0.d0; corr=0.d0; cov=0.d0
!        return
     enddo
  enddo


  write(*,*) '*****************************'
  write(*,*) 'wrote teleconnectivity'
  write(*,*) '*****************************'

  return
  stop
end subroutine teleconnectivity_noundef

subroutine  partialteleconnectivity(defx,defy,defz,undef,syr,eyr,dyr,var,clim, calmo,  nlev, smo, mlev, yrev,ptc, lontc, lattc, idx)
  implicit none
  integer :: ix,iy,it, iyr,&
       & k, j, n, &
       & defx, defy, defz,&
       & syr, eyr, dyr,&
       & imo, calmo, &
       & smo, mlev, nlev, yrev, &
       & irec, ierr, &
       & readcheck, &
       & sumcheck, &
       & mo_rnum, &
       & sit, eit

  real(4) :: &
       & clim(defx,defy, 14), &
       & sum1(defx,defy), &
       & corr(defx,defy), &
       & corr2(defx,defy), &
       & pcorr(defx,defy), &
       & ptc(defx,defy), lattc(defx,defy), lontc(defx,defy), &
       & mon(defx,defy), &
       & loc(2), &
       & var(defx,defy,dyr,12), &
       & idx(dyr,12), &
       & idxc(12)

  real :: temp,lat,lon, &
       & dx, dy, &
       & lonx, laty, &
       & undef, &
       & sum2, &
       & corr3, aveidx

  character (150) :: FileName1, FileName2

!  write(*,*) 'File No.=10 ',FileName1 
  write(*,*) "defx",defx,"defy",defy  
  write(*,*) 'defz',defz,'nlev',nlev,defx,defy

!  write(100,rec=nlev) ((clim(ix,iy,1),ix=1,defx),iy=1,defy)
!stop

! --------------------------------------------------
! cal Cliamte of index
! --------------------------------------------------
  idxc(:) = sum(idx(:,:), dim=1)/real(dyr)
  write(*,*) idxc
!------------------------------------------
! main program
!------------------------------------------
  dx=360.d0/real(defx,4)
  dy=180.d0/real(defy,4)

  if(mod(defy,2)/=0.d0)dy=180/(real(defy-1.d0,4))

  sum1=0.d0;  corr=0.d0;  sum2=0.d0; 
  corr2 = 0.d0; corr3 = 0.d0

  readcheck=0.d0
  sumcheck=0.d0
  k=1;j=1
!     write(*,*) 'k',k, 'j',j
!  do k=310,310
  do j=1,defy
     write(*,*) 'k',k, 'j',j
     do k=1,defx

        temp=0.0
        do it=1,dyr
           do imo=1,12

              if(calmo==14)then
                 if (imo==11 .or. imo==12 .or. imo==1 .or. imo==2 .or. imo==3)then
                    readcheck=1.d0
                 endif
              elseif(calmo==13)then
                 if (imo==12 .or. imo==1 .or. imo==2)then
                    readcheck=1.d0
                 endif
              else
                 if(calmo==imo)then
                    readcheck=1.d0
                 endif
              endif

              if(readcheck==1.d0)then
                 if(sumcheck/=1.d0)then
                    sum1(:,:)=sum1(:,:)+(var(:,:,it,imo)-clim(:,:,imo))**2
                    sum2 = sum2 + ((idx(it,imo)-idxc(imo))**2)
                    corr2 = corr2 + &
                      & (var(:,:,it,imo)-clim(:,:,imo))*&
                      & (idx(it,imo)-idxc(imo))
                 endif

                 corr(:,:)=corr(:,:)+&
                      & (var(:,:,it,imo)-clim(:,:,imo))*&
                      & (var(k,j,it,imo)-clim(k,j,imo))

                 corr3 = corr3+&
                      & (idx(it,imo)-idxc(imo))*&
                      & (var(k,j,it,imo)-clim(k,j,imo))

                 readcheck=0.d0
              endif
           enddo !imo
        enddo !it

        corr(:,:) = (corr(:,:))/(sqrt(sum1(:,:))*sqrt(sum1(k,j)))

        if(sumcheck==0)corr2(:,:) = ( corr2(:,:) / ( sqrt(sum1(:,:)) * (sqrt(sum2)) ))
        corr3 = (corr3 / ( sqrt(sum1(k,j)) * ( sqrt(sum2))) ) 
        pcorr(:,:) = (corr(:,:) - (corr2(:,:) * corr3)) / &
             & ( (sqrt(1.d0 - (corr2(:,:)**2)))* ( sqrt(1.d0 - (corr3**2))))

        if(sumcheck/=1.d0)then
           sumcheck=1.d0
        endif
        temp=minval(pcorr)
        loc=minloc(pcorr)
        lon=(real(loc(1),4)-1)*dx
        lat=(real(loc(2),4)-1)*dy-90.d0

        if(temp > 1.d0 .or. temp < -2.)then
           write(*,*) '*****************************************************'
           write(*,*) "Warning teleconnectivity dosen't have a negative value"
           write(*,*) '*****************************************************'
           write(*,*) 'corr',temp,k,j, loc(1),loc(2)
           temp=undef
        endif

        ptc(k,j)=temp
        lattc(k,j)=lat
        lontc(k,j)=lon
        temp=0.d0; corr=0.d0; corr3=0.d0
!        return
     enddo
  enddo


  write(*,*) '*****************************'
  write(*,*) 'wrote teleconnectivity'
  write(*,*) '*****************************'

  return
  stop
end subroutine partialteleconnectivity

subroutine  one_point_corr(defx,defy,defz,undef,syr,eyr,dyr,var,clim, calmo, nlev, smo, mlev, yrev, corr, reg, k, j, flg)
!subroutine one_point(defx,defy,defz,syr,eyr,dyr,clim,std, calmo, FileName1, FileName2, z_lev, k, j)
  implicit none
  integer :: ix,iy,it, iyr,&
       & k, j, n, &
       & defx, defy, defz,&
       & syr, eyr, dyr,&
       & imo, calmo, &
       & smo, mlev, nlev, yrev, &
       & irec, ierr, &
       & readcheck, &
       & sumcheck, &
       & mo_rnum, &
       & sit, eit

  integer(4) :: &
       & flg(defx,defy)

  real(4) :: &
       & clim(defx,defy, 14), &
       & sum1(defx,defy), &
       & sum2(defx,defy), &
       & corr(defx,defy), &
       & cov(defx,defy), &
       & reg(defx,defy), &
       & tc(defx,defy), lattc(defx,defy), lontc(defx,defy), &
       & mon(defx,defy), &
       & loc(2), &
       & var(defx,defy,dyr,12)

  real :: temp,lat,lon, &
       & dx, dy, &
       & lonx, laty, &
       & undef

  character (150) :: FileName1, FileName2

!  write(*,*) 'File No.=10 ',FileName1 
  write(*,*) "defx",defx,"defy",defy  
  write(*,*) 'defz',defz,'nlev',nlev,defx,defy

!------------------------------------------
! main program
!------------------------------------------
  dx=360.d0/real(defx,4)
  dy=180.d0/real(defy,4)

  if(mod(defy,2)/=0.d0)dy=180/(real(defy-1.d0,4))

  sum1=0.d0;  corr=0.d0; cov=0.d0
  readcheck=0.d0
  sumcheck=0.d0
  n=0.d0
  reg=0.d0

  temp=0.0
  do it=1,dyr
     do imo=1,12
        if(calmo==14)then
           if (imo==11 .or. imo==12 .or. imo==1 .or. imo==2 .or. imo==3)then
              readcheck=1.d0
           endif
        elseif(calmo==13)then
           if (imo==12 .or. imo==1 .or. imo==2)then
              readcheck=1.d0
           endif
        else
           if(calmo==imo)then
              readcheck=1.d0
           endif
        endif

        if(readcheck==1.d0)then
           n=n+1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           do ix=1, defx
              do iy=1, defy
                 if(flg(ix,iy) /= 100)then
                    sum1(ix,iy)=sum1(ix,iy)+(var(ix,iy,it,imo)-clim(ix,iy,imo))**2
                    cov(ix,iy)=cov(ix,iy)+&
                         & ((var(ix,iy,it,imo)-clim(ix,iy,imo))* & 
                         & (var(k,j,it,imo)-clim(k,j,imo)))
                 endif
              enddo
           enddo
!           sumidx = sumidx + (idx(it,imo) - idxc(imo))**2
           readcheck=0.d0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!           if(sumcheck/=1.d0) sum1(:,:)=sum1(:,:)+(var(:,:,it,imo)-clim(:,:,imo))**2
!           
!           corr(:,:)=corr(:,:)+&
!                & (var(:,:,it,imo)-clim(:,:,imo))*&
!                & (var(k,j,it,imo)-clim(k,j,imo))
!
!           reg(:,:)=reg(:,:)+&
!                & (var(:,:,it,imo)-clim(:,:,imo))*&
!                & (var(k,j,it,imo)-clim(k,j,imo))
!           
!!                 sum2(:,:)=sum2(:,:)+&
!!                      & (var(:,:,it,imo)-clim(:,:,imo))*&
!!                      & (var(k,j,it,imo)-clim(k,j,imo))
!
!           readcheck=0.d0
        endif
     enddo !imo
  enddo !it

  cov(:,:)=cov(:,:)/real(n-1)
  sum1(:,:)=sum1(:,:)/real(n-1)

  reg(:,:)  = cov(:,:)/ sum1(k,j)
  corr(:,:)=(cov(:,:))/(sqrt(sum1(:,:))*sqrt(sum1(k,j)))
  if(sumcheck/=1.d0)then
     sumcheck=1.d0
  endif

  do ix=1, defx
     do iy=1, defy
        if(flg(ix,iy) >= 10)then
           reg(ix,iy)=undef
           corr(ix,iy)=undef
        endif
     enddo
  enddo
!  reg(:,:)  = corr(:,:)/ sum1(:,:)
!  corr(:,:)=(corr(:,:))/(sqrt(sum1(:,:))*sqrt(sum1(k,j)))
!  if(sumcheck/=1.d0)then
!     sumcheck=1.d0
!  endif
!
  write(*,*) '*****************************'
  write(*,*) 'wrote One point correlation at ', dx*(k-1), dy*(j-1)-90.d0
  write(*,*) '*****************************'

  return
  stop
end subroutine one_point_corr

subroutine  one_point_corr_idx(defx,defy,defz,undef,syr,eyr,dyr,var,clim, calmo, &
     & nlev, smo, mlev, yrev, corr, reg,  idx, aveidx, flg)
  implicit none
  integer :: ix,iy,it, iyr,&
       &  n, &
       & defx, defy, defz,&
       & syr, eyr, dyr,&
       & imo, calmo, &
       & smo, mlev, nlev, yrev, &
       & irec, ierr, &
       & readcheck, &
       & sumcheck, &
       & mo_rnum, &
       & sit, eit

  real(4) :: &
       & clim(defx,defy, 14), &
       & sum1(defx,defy), &
       & sum2(defx,defy), &
       & corr(defx,defy), &
       & cov(defx,defy), &
       & reg(defx,defy), &
       & tc(defx,defy), lattc(defx,defy), lontc(defx,defy), &
       & mon(defx,defy), &
       & loc(2), &
       & var(defx,defy,dyr,12),&
       & idx(dyr,12), &
       & idxc(12)

  integer(4) :: &
       & flg(defx,defy)

  real :: temp, lat, lon, &
       & dx, dy, &
       & lonx, laty, &
       & undef, &
       & sumidx, aveidx

  character (150) :: FileName1, FileName2

!  write(*,*) 'File No.=10 ',FileName1 
  write(*,*) "defx",defx,"defy",defy  
  write(*,*) 'defz',defz,'nlev',nlev,defx,defy

!------------------------------------------
! main program
!------------------------------------------
  dx=360.d0/real(defx,4)
  dy=180.d0/real(defy,4)

  if(mod(defy,2)/=0.d0)dy=180/(real(defy-1.d0,4))

  idxc(:) = sum(idx(:,:), dim=1)/real(dyr)

  sum1=0.d0;  corr=0.d0;
  readcheck=0.d0
  sumcheck=0.d0

  reg=0.d0; sumidx=0.d0; sum1=0.d0
  cov=0.d0; n=0.d0

  temp=0.0
  do it=1,dyr
     do imo=1,12
        if(calmo==14)then
           if (imo==11 .or. imo==12 .or. imo==1 .or. imo==2 .or. imo==3)then
              readcheck=1.d0
           endif
        elseif(calmo==13)then
           if (imo==12 .or. imo==1 .or. imo==2)then
              readcheck=1.d0
           endif
        else
           if(calmo==imo)then
              readcheck=1.d0
           endif
        endif

        if(readcheck==1.d0)then
           n=n+1

           do ix=1, defx
              do iy=1, defy
!           if(sumcheck/=1.d0) sum1(:,:)=sum1(:,:)+(var(:,:,it,imo)-clim(:,:,imo))**2

!                 if(var(ix,iy,it,imo) /= undef)then
                 if(flg(ix,iy) /= 100)then
!                    sum1(:,:)=sum1(:,:)+(var(:,:,it,imo)-clim(:,:,imo))**2
!                    sumidx = sumidx + (idx(it,imo) - idxc(imo))**2
!!           sumidx = sumidx + (idx(it,imo) - idxc(imo))**2
!
!                    cov(:,:)=cov(:,:)+&
!                         & ((var(:,:,it,imo)-clim(:,:,imo))*&
!                         & (idx(it,imo) - idxc(imo)))

                    sum1(ix,iy)=sum1(ix,iy)+(var(ix,iy,it,imo)-clim(ix,iy,imo))**2
!           sumidx = sumidx + (idx(it,imo) - idxc(imo))**2
                    cov(ix,iy)=cov(ix,iy)+&
                         & ((var(ix,iy,it,imo)-clim(ix,iy,imo))*&
                         & (idx(it,imo) - idxc(imo)))

                 endif
!           write(*,*) "it=",it,"imo=",imo, idx(it,imo),var(1,1,it,imo)

              enddo
           enddo
           sumidx = sumidx + (idx(it,imo) - idxc(imo))**2
           readcheck=0.d0
        endif
     enddo !imo
  enddo !it

  write(*,*) "degree of freedom", n, aveidx

  cov(:,:)=cov(:,:)/real(n-1)
  sum1(:,:)=sum1(:,:)/real(n-1)
  sumidx=sumidx/real(n-1)

  reg(:,:)  = cov(:,:)/ sumidx
!  reg(:,:)  = corr(:,:)/ sum1(:,:)
  corr(:,:)=(cov(:,:))/(sqrt(sum1(:,:))*sqrt(sumidx))
  if(sumcheck/=1.d0)then
     sumcheck=1.d0
  endif

  do ix=1, defx
     do iy=1, defy
        if(flg(ix,iy) == 100)then
           reg(ix,iy)=undef
           corr(ix,iy)=undef
        endif
     enddo
  enddo

  write(*,*) '*****************************'
  write(*,*) 'wrote teleconnectivity'
  write(*,*) '*****************************'

  return
  stop
end subroutine one_point_corr_idx

subroutine eof

! program EOF
!program source from nishii-san's sample program.
  implicit none
! myear; 年数, mlat; 緯度の数
  integer,parameter :: myear=30, mlat=29 ,mlat2=29
  integer :: iyear,ilat,j
  real :: cor(mlat),reg(mlat)
  real :: ull(myear,mlat),vll(myear,mlat2)   !matrix
  real :: uall(myear,mlat),vall(myear,mlat2) !anomaly matrix
! 共分散行列のための配列
  real :: cov(mlat,mlat2),eigen_mat(mlat,mlat2) ! covariance, eigen_matrix
  real :: uznl(mlat),vznl(mlat2) ! time average
  integer :: un_iu
  character :: jobz,uplo
  integer :: info
  integer,parameter :: lda=29,n=29
  integer,parameter :: lwork=n*10
!  real :: a(lda,n), w(n), work(lwork)
  real :: w(n), work(lwork)
  integer :: i,ilda
! 固有値分解等で得られた固有ベクトル等の空間パターン
  real :: eigenV1(mlat),eigenV2(mlat)
! 時系列
  real :: TS(myear)
  real :: sgm,ave
  double precision :: pi
! 相関係数と回帰係数の緯度分布
! 偏差場行列の分散と平均(平均は０のはず)
  real :: vara(mlat),avea(mlat)
! 時系列の分散偏差と平均　(規格化されていれば，１と０)
  real :: varb,aveb
! 共分散 TS( PC time series ) & anomaly matrix.
!  real :: covab(myear)
  real :: covab(mlat)
! 相関と回帰係数



! 'V' の場合は固有値も固有ベクトルも計算。'N'の場合は固有値のみ計算
  jobz = 'V'
! 'U' の場合は上三角、'L'の場合は下三角の部分が蓄えられる。
  uplo = 'U'

!!!!  call SSYEV( jobz, uplo, n, a, lda, w, work, lwork, info)

!  call SSYEV( jobz, uplo, mlat, eigen_mat, mlat, w, work, lwork, info)

!固有値
  write(*,*) "Eigen values"
  j=1
  do i = mlat,1,-1
     write(*,*) i,w(i),100*w(i)/sum(w(:),dim=1)
!     eigenV(j)=w(i)
!     j=j+1
  enddo
  write(*,*) 
  
  write(*,*) "First eigen vector"
!第一固有ベクトル
  do i  = 1,mlat
     write(*,*) i,eigen_mat(i,mlat)
     eigenV1(i)=eigen_mat(i,mlat)
  enddo
  write(*,*) sum(eigenV1(:)*eigenV1(:),dim=1)
  
  write(30,rec=1) (eigenV1(i),i=1,mlat)

  write(*,*) 
  write(*,*) "Second eigen vector"
!第2固有ベクトル
  do i  = 1,mlat
     write(*,*) i,eigen_mat(i,mlat-1)
     eigenV2(i)=eigen_mat(i,mlat-1)
  enddo
  write(*,*) sum(eigenV2(:)*eigenV2(:),dim=1)
  
  write(30,rec=2) eigenV2(:)

!  write(*,*) 'dot_product',dot_product(eigenV1(:),eigenV2(:))
! 得られた空間パターン(固有ベクトル，特異値ベクトル等)を，
! 観測の偏差場との内積をとる(射影する)ことにより時系列を作成する．
! また，１標準偏差で規格化する．
! uall(myear,mlat) には偏差場行列，
! eigenV(mlat) には固有値ベクトル等の空間パターンのベクトルを代入しておく

! 内積により，時系列を求める
!　実際には偏差行列とベクトルのかけ算

  TS(:)=matmul(uall,eigenV1)
! 標準偏差を求める
  ave = 0.
  do iyear = 1,myear
     ave = ave + TS(iyear)
  enddo
  ave=ave/real(myear)

  sgm = 0.
  do iyear = 1,myear
     sgm = sgm + (TS(iyear)-ave)*(TS(iyear)-ave)
  enddo

  sgm=sqrt(sgm/real(myear))

!　規格化
  TS(:)=(TS(:)-ave)/sgm

!  do i=1,n
  write(40,rec=1) TS
!  enddo

  ave = 0.
  do iyear = 1,myear
     ave = ave + TS(iyear)
  enddo
  ave=ave/real(myear)

  sgm = 0.
  do iyear = 1,myear
     sgm = sgm + (TS(iyear)-ave)*(TS(iyear)-ave)
  enddo

  sgm=sqrt(sgm/real(myear))
  write(*,*) 0,ave,1,sgm

! uall とTS(myear) を代入しておく
  avea(:) = 0.
  aveb = 0.
  vara = 0.
  varb = 0.

  do iyear = 1,myear
     avea(:) = avea(:) + uall(iyear,:)
     aveb = aveb + TS(iyear)
  enddo

  do iyear = 1,myear
     vara(:) = vara(:) + (uall(iyear,:)-avea(:))*(uall(iyear,:)-avea(:))
     varb = varb + (TS(iyear)-aveb)*(TS(iyear)-aveb)
     covab(:) = covab(:) + (TS(iyear)-aveb)*(uall(iyear,:)-avea(:))
  enddo

  vara(:)=vara(:)/real(myear)
  varb=varb/real(myear)
  covab(:)=covab(:)/real(myear)

! 相関係数
  cor(:)=covab(:)/sqrt(vara(:)*varb)
! 回帰係数
  reg(:)=covab(:)/varb

!  do i=1,mlat
!     write(20,*) cor(i),reg(i)
!  enddo

  write(30,rec=3) cor
  write(30,rec=4) reg

end subroutine eof



! calculate distance two points. 
! used method of calculating inner product.
! need two points date : (lat,lon) (lat2,lon2) 
! back to distance : r 

subroutine cal_distance (lat, lon, Clat, Clon, r)
  implicit none 
  
  integer, parameter :: dims=3
  real,parameter :: &
       & Er=6.378E6, undef=-999
  real(4) ::  &
       & CC  (dims) , &
       & CCc (dims)
  real (4) ::  lat, lon, Clat, Clon, &
       & Rlon, Rlat, RClon, RClat, &
       & d2r, r2d, pi, inn_pro, r
  integer :: k
  
  pi=acos(-1.0d0)
  d2r=pi/180.0d0
  r2d=180.0d0/pi

  CCc=0.d0
  CC=0.d0

  Rlat=lat*d2r
  Rlon=lon*d2r
  RClat=Clat*d2r
  RClon=Clon*d2r

  CCc(1)=Er*cos(RClat)*cos(RClon)
  CCc(2)=Er*cos(RClat)*sin(RClon)
  CCc(3)=Er*sin(RClat)

  CC(1)=Er*cos(Rlat)*cos(Rlon)
  CC(2)=Er*cos(Rlat)*sin(Rlon)
  CC(3)=Er*sin(Rlat)

  inn_pro=0.d0
  do k=1,dims
     inn_pro=inn_pro + (CC(k)*CCc(k))
  enddo

  r=Er*acos(abs(inn_pro/(Er**2)))
!  write(6,*) abs(inn_pro/(Er**2))
  inn_pro=0.d0
  
  RETURN
endsubroutine cal_distance

