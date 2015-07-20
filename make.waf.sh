#! /bin/bash

rea=ncep

undef=-9.99E+20
defx=144; defy=73
f95file=make.waf.f90
model=plev
#year=194801-201204
year=194801-201111
mocom=12

switch=0

for z1z2crt in 0.25 #0.5
do
    z1z2=0

    if [ ${rea} = ncep ]; then
	syr=1; eyr=63; defz=17
	z_num=${defz}
	range="1000 925 850 700 600 500 400 300 250 200 150 100 70 50 30 20 10"
    fi

    for mocom in 13 #12 1 2 11 3 13
    do
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
	if [ ${mocom} = 13 ]; then
	    cmo=DJF
	fi
	if [ ${mocom} = 15 ]; then
	    cmo=Nov-Dec
	fi
	if [ ${mocom} = 16 ]; then
	    cmo=Dec-Jan
	fi
	if [ ${mocom} = 17 ]; then
	    cmo=Jan-Feb
	fi
	if [ ${mocom} = 18 ]; then
	    cmo=Feb-Mar
	fi

	dyr=`expr ${eyr} - ${syr} + 1`
	echo $dyr

	for lag in  0 -1 +1
	do
	    ofile=./waf_${rea}_${mocom}lag${lag}_DJFwp1idx_${year}

	    z_lev=1; lev=1000; defx=144; defy=73

	    ####################
	    exe=waf_${vari}${lev}.out
	    if [ ${z1z2} = 0 ];then

	    ifileZ=./data/${cmo}_cmp/plev/cmp_anm_NCEP_Z_${mocom}lag${lag}_wpidx1.0_${year}.bin
	    ifileU=./data/${cmo}_cmp/plev/cmp_anm_NCEP_U_${mocom}lag${lag}_wpidx1.0_${year}.bin
	    ifileV=./data/${cmo}_cmp/plev/cmp_anm_NCEP_V_${mocom}lag${lag}_wpidx1.0_${year}.bin
	    ifileT=./data/${cmo}_cmp/plev/cmp_anm_NCEP_T_${mocom}lag${lag}_wpidx1.0_${year}.bin

	    elif [ ${z1z2} = 1 ]; then
		ifileZ=./${cmo}_composite/ncep/plev/data/composite_anomaly_NCEP_Z_${mocom}lag${lag}_z1z2${z1z2crt}_${year}.bin
		ifileU=./${cmo}_composite/ncep/plev/data/composite_anomaly_NCEP_U_${mocom}lag${lag}_z1z2${z1z2crt}_${year}.bin
		ifileV=./${cmo}_composite/ncep/plev/data/composite_anomaly_NCEP_V_${mocom}lag${lag}_z1z2${z1z2crt}_${year}.bin
		ifileT=./${cmo}_composite/ncep/plev/data/composite_anomaly_NCEP_T_${mocom}lag${lag}_z1z2${z1z2crt}_${year}.bin

	    fi

	    plevfile=./def/${rea}_plev.txt
	    echo ${plevfile}
	    if [ -e "a.out" ]; then
		rm ./a.out
	    fi
	    #	ifort -assume byterecl -nozero -o  ${exe} ${f95file}
	    f95 -fbounds-check -o ${exe} ${f95file}

	    cat <<EOF | ./${exe}
${undef}
${defx}
${defy}
${defz}
${syr}
${eyr}
${dyr}
${lev}
${z_lev}
${surface}
${lag}
${mocom}
${plevfile}
${ifileZ}
${ifileT}
${ifileU}
${ifileV}
${ofile}.bin
EOF

	    z_lev=`expr ${z_lev} + 1`
	    echo "z_lev" ${z_lev}

	    rm ${exe}

	    ctlfile=${ofile}.ctl
	    cat <<EOF > ${ctlfile}
dset ^${ofile}.bin
options little_endian
undef ${undef}
xdef ${defx} linear 0.0 2.5
ydef ${defy} linear -90 2.5
zdef ${defz} levels ${range}
tdef 1 linear apr1948 1mo
vars   34
powafx    ${defz} 999 some variable
powafy    ${defz} 999 some variable
powafz    ${defz} 999 some variable
powafzp   ${defz} 999 some variable
pou1 ${defz} 999 some variable
pou2 ${defz} 999 some variable
pou3 ${defz} 999 some variable
pou4 ${defz} 999 some variable
pov1 ${defz} 999 some variable
pov2 ${defz} 999 some variable
pov3 ${defz} 999 some variable
pov4 ${defz} 999 some variable
poz1 ${defz} 999 some variable
poz2 ${defz} 999 some variable
poz3 ${defz} 999 some variable
poz4 ${defz} 999 some variable
pok2 ${defz} 999 refractive index in positive
newafx    ${defz} 999 some variable
newafy    ${defz} 999 some variable
newafz    ${defz} 999 some variable
newafzp   ${defz} 999 some variable
neu1 ${defz} 999 some variable
neu2 ${defz} 999 some variable
neu3 ${defz} 999 some variable
neu4 ${defz} 999 some variable
nev1 ${defz} 999 some variable
nev2 ${defz} 999 some variable
nev3 ${defz} 999 some variable
nev4 ${defz} 999 some variable
nez1 ${defz} 999 some variable
nez2 ${defz} 999 some variable
nez3 ${defz} 999 some variable
nez4 ${defz} 999 some variable
nek2 ${defz} 999 refractive index in positive
endvars
EOF

    mv ${ofile}.ctl  ${ofile}.bin ./data/${cmo}_cmp/plev/

    if [ ${mocom} = 13 ]; then
	break
    fi
    done
done
done
exit
