#! /bin/bash

#rea=ncep JRA25
rea=ERAi

undef=-9.99E+20
#defx=144;defy=73
defx=480;defy=241; dx=0.75; dy=0.75
defx=144;defy=73; dx=2.5; dy=2.5
f95file=make.energy_conversion.f90
model=plev

year=198182-200001

mocom=12

trap signalExit 2
function signalExit(){
echo "signal recieved"
exit
}

crt_switch=3
if [ ${crt_switch} = 1 ];then
    num_crt=1.0
    wpcrt=1.0
    ccrt=wpidx
elif [ ${crt_switch} = 2 ];then
    num_crt=0.25
    ccrt=z1z2
elif [ ${crt_switch} = 3 ];then
    num_crt=""
    ccrt=wppc1u
fi


switch=0



z1z2=1

if [ ${rea} = ncep ]; then
    syr=1
    eyr=63
    defz=17
    z_num=${defz}
    range="1000 925 850 700 600 500 400 300 250 200 150 100 70 50 30 20 10"
fi
if [ ${rea} = JRA25 ]; then
    syr=1
    eyr=63
    defz=23
    range="1000 925 850 700 600 500 400 300 250 200 150 100 70 50 30 20 10 7 5 3 2 1 0.4 "
fi
if [ ${rea} = ERAi ]; then
    syr=1
    eyr=32
    defz=37
    range="1000 975 950 925 900 875 850 825 800 775 750 700 650 600 550 500 450 400 350 300 250 225 200 175 150 125 100 70 50 30 20 10 7 5 3 2 1"
fi

for mocom in 13 #12 1 2 3 11 13
do
    cmo=(0 "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec" "DJF" "NDJFM" "Nov-Dec" "Dec-Jan" "Jan-Feb" "Feb-Mar" "a" "MAM" "JJA" "SON" )

    dyr=`expr ${eyr} - ${syr} + 1`
    echo $dyr

    for lag in 0 #-1 +1
    do
	ofile=./energetics_wp_${rea}_${mocom}lag${lag}_${ccrt}${num_crt}_${year}

	z_lev=1
	lev=1000


	####################
	exe=EnergyConversion_${vari}${lev}.out

	ifz=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_z_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin
	ifU=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_u_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin
	ifV=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_v_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin
	ifT=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_T_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin
	ifomega=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_w_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin
	ifQ1=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_Q1_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin

	ifUU=../../../../../ncep/wp/cmp/monthly/DJF_cmp/plev/data/cmp_anm_NCEP_UU_13lag0_wpidx1.0_197901-200206.ctl
	ifUV=../../../../../ncep/wp/cmp/monthly/DJF_cmp/plev/data/cmp_anm_NCEP_UV_13lag0_wpidx1.0_197901-200206.ctl
	ifVV=../../../../../ncep/wp/cmp/monthly/DJF_cmp/plev/data/cmp_anm_NCEP_VV_13lag0_wpidx1.0_197901-200206.ctl
	ifUT=../../../../../ncep/wp/cmp/monthly/DJF_cmp/plev/data/cmp_anm_NCEP_UT_13lag0_wpidx1.0_197901-200206.ctl
	ifVT=../../../../../ncep/wp/cmp/monthly/DJF_cmp/plev/data/cmp_anm_NCEP_VT_13lag0_wpidx1.0_197901-200206.ctl
	ifwT=../../../../../ncep/wp/cmp/monthly/DJF_cmp/plev/data/cmp_anm_NCEP_wT_13lag0_wpidx1.0_197901-200206.ctl

	ifUUh8=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_UUh8_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin
	ifUVh8=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_UVh8_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin
	ifVVh8=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_VVh8_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin
	ifUTh8=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_UTh8_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin
	ifVTh8=./data/${cmo[${mocom}]}_cmp/plev${dx}/cmp_anm_${rea}_VTh8_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin
	ifwTh8=../../../../../ncep/wp/cmp/monthly/DJF_cmp/plev/data/cmp_anm_NCEP_wTh8_13lag0_wpidx1.0_197901-200206.ctl

	ifps=./data/${cmo[${mocom}]}_cmp/surface${dx}/cmp_anm_${rea}_sp_${mocom}lag${lag}_${ccrt}${num_crt}_${year}.bin

	plevfile=/home/eady4/sho/study/def/${rea}_plev.txt
	efffile=./txt/efficiency/${cmo[${mocom}]}_z1z2${z1z2crt}_Energetics_efficiency.txt
	if [ ${crt_switch} = 1 ]; then
	    efffile=./txt/efficiency/${cmo[${mocom}]}_wpindex${wpcrt}_Energetics_efficiency.txt
	fi

	echo ${plevfile}

	if [ -e "a.out" ]; then
	    rm ./a.out
	fi
	echo ${ifT}
	obj=./object
	ifort -assume byterecl -nozero -o ${exe} ${obj}rf.o ${obj}statics.o ${f95file}
	# ifort -assume byterecl -nozero -o  ${exe} ${f95file}
	# f95  -o ${exe} ${f95file}

	cat <<EOF | ./${exe}
${undef}
${defx}
${defy}
${defz}
${syr}
${eyr}
${dyr}
${lag}
${mocom}
${plevfile}
${ifz}
${ifT}
${ifU}
${ifV}
${ifomega}
${ifQ1}
${ifps}
${ifUU}
${ifUV}
${ifVV}
${ifUT}
${ifVT}
${ifwT}
${ifUUh8}
${ifUVh8}
${ifVVh8}
${ifUTh8}
${ifVTh8}
${ifwTh8}
${ofile}.bin
${efffile}
EOF


	z_lev=`expr ${z_lev} + 1`
	echo "z_lev" ${z_lev}

	if [ ! -e ${ofile}.bin ]; then
	    exit
	fi

	rm ${exe}

	ctlfile=${ofile}.ctl
	cat <<EOF > ${ctlfile}
dset ^${ofile}.bin
options little_endian
undef ${undef}
xdef ${defx} linear 0.0 ${dx}
ydef ${defy} linear -90 ${dy}
zdef ${defz} levels ${range} 
tdef 1 linear apr1948 1mo
vars   40
pock_x  ${defz} 999 barotropic energy conversion 
pock_y  ${defz} 999 barotropic energy conversion
pock_xnp  ${defz} 999 barotropic energy conversion 
pock_ynp  ${defz} 999 barotropic energy conversion
pocp_x   ${defz} 999 d w pt dp 
pocp_y   ${defz} 999 d w pt dp 
pocp_xnp   ${defz} 999 d w pt dp 
pocp_ynp   ${defz} 999 d w pt dp 
poke    ${defz} 999 d w pt dp 
poape   ${defz} 999 d w pt dp 
pocq   ${defz} 999 d w pt dp
posp   ${defz} 999 d w pt dp 
poketoape ${defz} 999 conversion from KE' to APE'
poEPx ${defz} 999 extended EP flux  of x compornent (v2-u2)
poEPy ${defz} 999 extended EP flux of ycomponent (uv)
pockuh8 ${defz} 999 ck by transient eddy uu, uv
pockvh8 ${defz} 999 ck by transient eddy uv. vv
pocpuh8 ${defz} 999 cp by transient eddy ut
pocpvh8 ${defz} 999 cp by transient eddy vt
pock_xnp2  ${defz} 999 barotropic energy conversion 
neck_x  ${defz} 999 barotropic energy conversion 
neck_y  ${defz} 999 barotropic energy conversion
neck_xnp  ${defz} 999 barotropic energy conversion 
neck_ynp  ${defz} 999 barotropic energy conversion
necp_x   ${defz} 999 d w pt dp 
necp_y   ${defz} 999 d w pt dp 
necp_xnp   ${defz} 999 d w pt dp 
necp_ynp   ${defz} 999 d w pt dp 
neke    ${defz} 999 d w pt dp 
neape   ${defz} 999 d w pt dp 
necq   ${defz} 999 d w pt dp
nesp   ${defz} 999 d w pt dp 
neketoape ${defz} 999 conversion from KE' to APE'
neEPx ${defz} 999 extended EP flux  of x compornent (v2-u2)
neEPy ${defz} 999 extended EP flux of ycomponent (uv)
neckuh8 ${defz} 999 ck by transient eddy uu, uv
neckvh8 ${defz} 999 ck by transient eddy uv. vv
necpuh8 ${defz} 999 cp by transient eddy ut
necpvh8 ${defz} 999 cp by transient eddy vt
neck_xnp2  ${defz} 999 barotropic energy conversion 
endvars
EOF

	mv ${ofile}.ctl  ${ofile}.bin ./data/${cmo[${mocom}]}_cmp/plev${dx}/

	if [ ${mocom} = 13 ]; then
	    break
	fi

    done
done
exit




exit
