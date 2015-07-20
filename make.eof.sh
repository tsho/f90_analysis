#! /bin/bash

#bin=report1.bin
#rm -f ${bin}

undef=-9.99E+20
rea=ncep
defx=144; defy=73; dx=2.5; dy=2.5
smo=1; defz=37
f95file=sub.eof.f90
#f95file=test.sub.eof.f90

iyear=194801-201204
year=194801-201111; syear=1948; smo=1; le=1
year=197980-200405; syear=1979; smo=1; le=1 rcp=""
year=207980-209900; syear=2079; smo=1; le=1 
rcp="_rcp45"; idir="../CMIP5/rcp45/atm.gr2.5x2.5/mo"

#jan1979
model=plev

#syear=1979; smo=1; le=1


mocom=12
switch=0


for mocom in 13 #3
#21 13 14
do

    mocom1=${mocom}
    mocom2=-999
    mocom3=-999

    if [ ${mocom} = 13 ]; then
	mocom1=12; 	mocom2=1; 	mocom3=2
	cmo=DJF
    fi
    if [ ${mocom} = 21 ]; then
	mocom1=6; 	mocom2=7; 	mocom3=8
	cmo=JJA
    fi

    if [ ${mocom} = 12 ]; then
	cmo=Dec
    fi
    if [ ${mocom} = 1 ]; then
	cmo=Jan
    fi
    if [ ${mocom} = 2 ]; then
	cmo=Feb
    fi
    if [ ${mocom} = 3 ]; then
	cmo=Mar
    fi
    if [ ${mocom} = 11 ]; then
	cmo=Nov
    fi

    for model in HadGEM2-ES HadGEM2-CC HadGEM2-AO bcc-csm1-1 NorESM1-ME MPI-ESM-LR MIROC4h inmcm4 HadCM3 GFDL-ESM2M FGOALS-s2 CMCC-CM CanESM2 bcc-csm1-1 NorESM1-M MIROC-ESM-CHEM IPSL-CM5B-LR HadGEM2-ES GISS-E2-R-CC GFDL-ESM2G CSIRO-Mk3-6-0 CESM1-CAM5 CanCM4 ACCESS1-3 MRI-CGCM3 MIROC-ESM IPSL-CM5A-MR HadGEM2-CC GISS-E2-H-CC GFDL-CM3 CNRM-CM5 CESM1-BGC BNU-ESM ACCESS1-0 MPI-ESM-MR MIROC5 IPSL-CM5A-LR HadGEM2-AO GISS-E2-H FIO-ESM CMCC-CMS CCSM4  bcc-csm1-1-m
#FULL hist: NorESM1-ME  MPI-ESM-MR MIROC5 IPSL-CM5A-LR HadGEM2-AO GISS-E2-H FIO-ESM CMCC-CMS CESM1-FASTCHEM CanESM2 bcc-csm1-1 NorESM1-M MPI-ESM-LR  MIROC4h inmcm4 HadCM3 GFDL-ESM2M FGOALS-s2 CMCC-CM CESM1-CAM5 CanCM4  ACCESS1-3 MRI-CGCM3 MIROC-ESM-CHEM IPSL-CM5B-LR HadGEM2-ES GISS-E2-R-CC GFDL-ESM2G CSIRO-Mk3-6-0 CMCC-CESM CESM1-BGC BNU-ESM ACCESS1-0 MPI-ESM-P MIROC-ESM  IPSL-CM5A-MR HadGEM2-CC GISS-E2-H-CC GFDL-CM3 CNRM-CM5  CESM1-WACCM CCSM4 bcc-csm1-1-m FGOALS-g2
#run45: NorESM1-ME MPI-ESM-LR MIROC4h inmcm4 HadCM3 GFDL-ESM2M FGOALS-s2 CMCC-CM CanESM2 bcc-csm1-1 NorESM1-M MIROC-ESM-CHEM IPSL-CM5B-LR HadGEM2-ES GISS-E2-R-CC GFDL-ESM2G CSIRO-Mk3-6-0 CESM1-CAM5 CanCM4 ACCESS1-3 MRI-CGCM3 MIROC-ESM IPSL-CM5A-MR HadGEM2-CC GISS-E2-H-CC GFDL-CM3 CNRM-CM5 CESM1-BGC BNU-ESM ACCESS1-0 MPI-ESM-MR MIROC5 IPSL-CM5A-LR HadGEM2-AO GISS-E2-H FIO-ESM CMCC-CMS CCSM4  bcc-csm1-1-m
    do

	for vari in zg
	do
	    surface=0
	    gauss=0
	    #	defx=480; defy=241; mlev=0;  dx=0.75; dy=0.75
	    defx=144; defy=73; mlev=0;  dx=2.5; dy=2.5

	    if [ ${vari} = sp ] || [ ${vari} = msl ] || [ ${vari} = skt ] || [ ${vari} = sshf ] || [ ${vari} =  sst ] || [ ${vari} =  tp ] || [ ${vari} = lsp ] || [ ${vari} = u10 ] || [ ${vari} = v10 ] || [ ${vari} = Thlev60 ] || [ ${vari} = Uhlev60 ] || [ ${vari} = Vhlev60 ] || [ ${vari} = gpcp ] || [ ${vari} = OISST ]; then
	    surface=1
	    fi
	    if [ ${year} = '194801-201111' ]; then
		syr=1948; eyr=2011; dyr=63
		# dyr=`expr ${eyr} - ${syr} + 1`
	    fi
	    if [ ${year} = '197901-200106' ]; then
		syr=1979; eyr=2001; dyr=22
		# 1979/80 - 2011/12 eyr=33
	    fi
	    if [ ${year} = '197980-200405' ]; then
		syr=1979; eyr=2005; dyr=26
		# 1979/80 - 2004/05 eyr=33
	    fi
	    if [ ${year} = '207980-209900' ]; then
		syr=2079; eyr=2100; dyr=21
		# 1979/80 - 2004/05 eyr=33
	    fi

	    echo $dyr

	    #	range="1000 925 850 700 600 500 400 300 250 200 150 100 70 50 30 20 10"
	    range="500"
	    defz=17

	    ncf=0

	    if [ ${vari} = zg ] || [ ${vari} = T ] || [ ${vari} = u ] || [ ${vari} = v ] || [ ${vari} = w ] || [ ${vari} = sp ] || [ ${vari} = msl ] || [ ${vari} = skt ] || [ ${vari} = gpcp ] || [ ${vari} =  sst ] || [ ${vari} =  tp ] || [ ${vari} = lsp ] || [ ${vari} = u10 ] || [ ${vari} = v10 ]; then
		ncf=1
	    fi

	    for lag in 0 
	    do
		z_lev=1
		z_lev=6
		for lev in ${range}
		do

		    ifile=./data/gr/${model}${vari}${lev}.monthly-mean.gr
		    if [ $ncf = 1 ] && [ ! -e $ifile ]; then

			deft=406
			# undef=-9.99E+8

			ictl="../CMIP5/historical/atm.gr2.5x2.5/mo/${model}/run1/zg.ctl"
			ictl="../CMIP5/${rcp}/atm.gr2.5x2.5/mo/${model}/run1/zg.ctl"
			ictl="${idir}/${model}/run1/zg.ctl"

			gsfile=temp.save.gs
			if [ ! -e $ictl ]; then
			    echo "No exist ctl file!, $model, $ictl"
			    exit
			fi
			if [ -e $gsfile ]; then
			    /bin/rm $gsfile
			fi
			cat <<EOF > ${gsfile}
  if(${vari} != gpcp) 
    if(${surface} = 1); 'open ${ictl}'; endif
    if(${surface} = 0); 'open ${ictl}'; endif
  endif
  if(${vari} = gpcp); 'sdfopen /e4b/GPCPv2/monthly/nc/precip.mon.mean.nc'; endif

  'set gxout fwrite'
  'set undef ${undef}'
*  'set undef dfile'
  'set fwrite -le -st ${ifile}'

g=9.80665

  'set x 1 ${defx}'
  'set y 1 ${defy}'
  'set time 00z01jan${syr} 00z01dec${eyr}'
*  'set lev ${lev}'
  'set z ${z_lev}'

  if(${vari} != gpcp) 
   if (${vari} = z); 'd ${vari}'; endif
   if (${vari} != z); 'd ${vari}'; endif
  endif
   if (${vari} = gpcp); 'd precip'; endif
  'disable fwrite'
  'reinit'
  'quit'
EOF
			grads -b -lc ${gsfile}
			echo "convert from nc to grads file!!", ${vari}${lev}
			# rm ./${gsfile}
		    fi

		    yrev=0

		    if [ ! -e ${input_file} ]; then
			echo ${input_file}
			echo "not exist"
			exit
		    fi

		    #DJF composite NOT calculate lag composite
		    if [ ${mocom} = 13 ] ; then
			if [ ${lag} = +1 ] || [ ${lag} = -1 ]; then
			    break
			fi
		    fi

		    #lat 20 ~ 70, lon 120 ~ 240
		    defx2=25; defy2=21
		    lon1=120; lon2=180
		    lat1=20; lat2=70

		    ofile_ev=./eof_ev_${model}${rcp}_${vari}_${mocom}_x${lon1}${lon2}_y${lat1}${lat2}_${year}
		    ofile_corr=./eof_corr_${model}${rcp}_${vari}_${mocom}_x${lon1}${lon2}_y${lat1}${lat2}_${year}
		    ofile_reg=./eof_reg_${model}${rcp}_${vari}_${mocom}_x${lon1}${lon2}_y${lat1}${lat2}_${year}
		    ofile_pc=./eof_pc_${model}${rcp}_${vari}_${mocom}_x${lon1}${lon2}_y${lat1}${lat2}_${year}
		    ofile_pc_allmo=./eof_pc_all_${model}${rcp}_${vari}_${mocom}_x${lon1}${lon2}_y${lat1}${lat2}_${year}
		    ofile_txt=./eof_domain_${model}${rcp}_${vari}_${mocom}_x${lon1}${lon2}_y${lat1}${lat2}_${year}
		    exe=eof_${vari}${lev}.${model}${rcp}.out

		    if [ ${vari} = Z ]  && [ ${z_lev} = 1 ] && [ ${lev} = 500 ]; then
			z_lev=6
		    fi

		    if [ -e ${exe} ]; then
			rm ${exe}
		    fi

		    obj=./object
		    ifort  -traceback -assume byterecl -nozero -lblas -llapack -o  ${exe} ${f95file} ${obj}/rf.o ${obj}/statics.o
		    #	    ifort -assume byterecl -nozero -o ${exe} ${obj}rf.o ${obj}statics.o ${f95fil}eo
		    #f95  -o ${exe} ${f95file}
		    #f95 -fbacktrace -o ${exe} ${f95file} ${obj}rf.o ${obj}statics.o
		    cat <<EOF | ./${exe}
${undef}
${defx}
${defy}
${defz}
${defx2}
${defy2}
${lon1}
${lon2}
${lat1}
${lat2}
${dx}
${dy}
${syr}
${syear}
${eyr}
${dyr}
${z_lev}
${surface}
${mocom}
${mocom1}
${mocom2}
${mocom3}
${le}
${smo}
${mlev}
${yrev}
${ifile}
${ofile_ev}.bin
${ofile_pc}.bin
${ofile_corr}.bin
${ofile_reg}.bin
${ofile_pc_allmo}.bin
${ofile_txt}.txt
EOF

		    #cat report1_u${lev}_m${mon}.bin >> ${bin} 
		    #dEOF1=`awk 'NR==32 {print $4}' ${txt} | cut -c 1-4`
		    #dEOF2=`awk 'NR==33 {print $4}' ${txt} | cut -c 1-4`
		    #echo ${dEOF1} ${dEOF2} 

		    deft=${dyr}
		    if [ ${mocom} = 13 ]; then
			deft=`expr ${dyr} \* 3`
			cmo=Dec
		    fi
		    echo $deft

		    ctlfile=${ofile_ev}.ctl
		    cat <<EOF > ${ctlfile}
dset ^${ofile_ev}.bin
options little_endian 
undef ${undef}
xdef ${defx2} linear ${lon1} ${dx}
ydef ${defy2} linear ${lat1} ${dy}
zdef   1 linear 0 1
tdef   3 linear 01${cmo}${syr} 12mo
vars   1
ev   1  999 EOF eigen vector (lat $lat1 $lat2, lon ${lon1} $lon2)
endvars
EOF

		    ctlfile=${ofile_corr}.ctl
		    cat <<EOF > ${ctlfile}
dset ^${ofile_corr}.bin
options little_endian 
undef ${undef}
xdef ${defx2} linear ${lon1} ${dx}
ydef ${defy2} linear ${lat1} ${dy}
zdef   1 linear 0 1
tdef   3 linear 01${cmo}${syr} 12mo
vars  1
cor   1  999 corr (lat $lat1 $lat2, lon ${lon1} $lon2)
endvars
EOF

		    ctlfile=${ofile_reg}.ctl
		    cat <<EOF > ${ctlfile}
dset ^${ofile_reg}.bin
options little_endian 
undef ${undef}
xdef ${defx2} linear ${lon1} ${dx}
ydef ${defy2} linear ${lat1} ${dy}
zdef   1 linear 0 1
tdef   3 linear 01${cmo}${syr} 12mo
vars   1
reg   1  999 reg (lat $lat1 $lat2, lon ${lon1} $lon2)
endvars
EOF

		    ctlfile=${ofile_pc}.ctl
		    cat <<EOF > ${ctlfile}
dset ^${ofile_pc}.bin
options little_endian 
undef ${undef}
xdef 1 linear 0 1
ydef 1 linear 0 1
zdef 3 linear 0 1
tdef ${deft} linear 01${cmo}${syr} 1yr
vars   1
pc   3  999 PC (lat $lat1 $lat2, lon ${lon1} $lon2)
endvars
EOF

		    if [ ${mocom} = 13 ]; then
			cmo=DJF
			deft=`expr \( ${dyr} + 1 \) \* 12`
			echo $deft
			ctlfile=${ofile_pc_allmo}.ctl
			cat <<EOF > ${ctlfile}
dset ^${ofile_pc_allmo}.bin
options little_endian 
undef ${undef}
xdef 1 linear 0 1
ydef 1 linear 0 1
zdef 3 linear 0 1
tdef ${deft} linear 01jan${syr} 1mo
vars   1
pc   3  999 PC (lat $lat1 $lat2, lon ${lon1} $lon2)
endvars
EOF
		    fi

		    dir="./data/${cmo}/${model}${rcp}/"
		    if [ ! -e ${dir} ]; then
			mkdir -p $dir
			echo "mkdir $dir"
		    fi

		    mv ./${ofile_corr}.bin ./${ofile_ev}.bin ./${ofile_corr}.ctl ./${ofile_ev}.ctl $dir
		    mv ./${ofile_pc_allmo}.bin ./${ofile_pc_allmo}.ctl  $dir
		    mv ./${ofile_pc}.bin ./${ofile_pc}.ctl  $dir
		    mv ./${ofile_reg}.bin ./${ofile_reg}.ctl  $dir
		    mv ./${ofile_txt}.txt $dir
		    rm $exe $ifile

		    echo $model $level $vari
		    echo "finish calculate"
		done #lev
	    done #lag
	done #vari
    done #model
done #calmo 13(DJF)
exit
