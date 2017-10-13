#!/bin/bash

# From http://stackoverflow.com/a/246128/1876449
# ----------------------------------------------
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
SCRIPTDIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

# -----------------
# Detect usual bits
# -----------------

ARCH=$(uname -s)
MACH=$(uname -m)
NODE=$(uname -n)

# ------------------------------
# Define an in-place sed command
# because Mac sed is stupid old
# ------------------------------

if [[ $ARCH == Darwin ]]
then
   SED='sed -i '' '
else
   SED='sed -i'
fi

# -----
# Usage
# -----

usage ()
{
   echo "Usage: $0 [OPSB] [OPSBIO] [6HR] [0DY] [2DY] [5DY] [10DY] [1MO] [MEM] [MINMAX] [CHOU] [RRTMG] [HEMCO] [REPLAY] [BENCH] [BRO] [BROELE] [PGI] [SATSIM] [IOS] [NOHIS] [HISTORY] [TINY] [HUGE] [RRTMG_SW] [RRTMG_LW] [LINK]  [G40] [H50] [NC4] [BIN] [WW3] [MIC] [DAS] [POLICE]"
   echo ""
   echo " Common Well-Tested Options "
   echo " ========================== "
   echo "       OPSB: Make an ops-bench experiment"
   echo "     OPSBIO: Make an ops-bench experiment with IOserver"
   echo "    OPSPORT: Make a portable ops-bench experiment"
   echo "        6HR: Make a six-hour experiment"
   echo "        0DY: Make a zero-day experiment"
   echo "        2DY: Make a two-day experiment"
   echo "        5DY: Make a five-day experiment"
   echo "       10DY: Make a ten-day experiment"
   echo "        1MO: Use single-moment moist"
   echo "        2MO: Use two-moment moist"
   echo "        MEM: Add memory stats"
   echo "     MINMAX: Use minmax timers"
   echo "       CHOU: Enable both shortwave and longwave Chou code"
   echo "      RRTMG: Enable both shortwave and longwave RRTMG code"
   echo "      HEMCO: Enable HEMCO"
   echo "     REPLAY: Turn on regular replay"
   echo "      BENCH: Use benchmark qos"
   echo "    NOCHECK: Don't checkpoint"
   echo "        BRO: Turn on Broadwell at NAS"
   echo "     BROELE: Turn on Broadwell at NAS (Electra)"
   echo "        PGI: Use 12 cores per node"
   echo "     SATSIM: Turn on SATSIM (ISCCP)"
   echo "        IOS: Turn on IOSERVER"
   echo ""
   echo " Less Common Options "
   echo " =================== "
   echo "      NOHIS: Enables no HISTORY output"
   echo "    HISTORY: Enables smaller HISTORY.rc"
   echo "       TINY: Use minimal BCs"
   echo "       HUGE: Use huge BCs"
   echo "   RRTMG_SW: Enable shortwave RRTMG code"
   echo "   RRTMG_LW: Enable longwave RRTMG code"
   echo "       LINK: Link restarts in scratch"
   echo ""
   echo " Rare or Obsolete Options (Use at your own risk) "
   echo " =============================================== "
   echo "        G40: Use Ganymed-4_0 directories"
   echo "        H50: Use Heracles-5_0 directories"
   echo "        NC4: Convert to use NC4 restarts"
   echo "        BIN: Convert to use binary restarts"
   echo "        WW3: Turn on WAVEWATCH III capabilities"
   echo "        MIC: Enable MIC code"
   echo "     POLICE: Enables PoliceMe functionality"
   echo "        DAS: Add DAS SBATCH pragmas"
   echo ""
   echo " Note: Lowercase also allowed"
}

# ------------------------------
# Process command line arguments
# ------------------------------

# Set defaults
# ------------

USEG40=FALSE
USEH50=FALSE
OPSB=FALSE
OPSBIO=FALSE
OPSPORT=FALSE
SIXHOUR=FALSE
ZERODAY=FALSE
TWODAY=FALSE
FIVEDAY=FALSE
TENDAY=FALSE
ONEMO=FALSE
TWOMO=FALSE
NC4=FALSE
BIN=FALSE
WW3=FALSE
MEM=FALSE
MINMAX=FALSE
TINY=FALSE
HUGE=FALSE
MIC=FALSE
CHOU=FALSE
RRTMG=FALSE
RRTMG_SW=FALSE
RRTMG_LW=FALSE
NOHIS=FALSE
HISTORY=FALSE
POLICE=FALSE
LINK=FALSE
DAS=FALSE
BENCH=FALSE
NOCHECK=FALSE
REPLAY=FALSE
BRO=FALSE
BROELE=FALSE
PGI=FALSE
SATSIM=FALSE
IOS=FALSE
HEMCO=FALSE

while [ "${1+defined}" ]
do
   case "$1" in
      "G40" | "g40")
         USEG40=TRUE
         shift
         ;;
      "H50" | "h50")
         USEH50=TRUE
         shift
         ;;
      "OPSB" | "opsb")
         OPSB=TRUE
         shift
         ;;
      "OPSBIO" | "opsbio")
         OPSBIO=TRUE
         shift
         ;;
      "OPSPORT" | "opsport")
         OPSPORT=TRUE
         shift
         ;;
      "6HR" | "6hr")
         SIXHOUR=TRUE
         shift
         ;;
      "0DY" | "0dy")
         ZERODAY=TRUE
         shift
         ;;
      "2DY" | "2dy")
         TWODAY=TRUE
         shift
         ;;
      "5DY" | "5dy")
         FIVEDAY=TRUE
         shift
         ;;
      "10DY" | "10dy")
         TENDAY=TRUE
         shift
         ;;
      "1MO" | "1mo")
         ONEMO=TRUE
         shift
         ;;
      "2MO" | "2mo")
         TWOMO=TRUE
         shift
         ;;
      "NC4" | "nc4")
         NC4=TRUE
         shift
         ;;
      "BIN" | "bin")
         BIN=TRUE
         shift
         ;;
      "MEM" | "mem")
         MEM=TRUE
         shift
         ;;
      "MINMAX" | "minmax")
         MINMAX=TRUE
         shift
         ;;
      "TINY" | "tiny" | "MIN" | "min")
         TINY=TRUE
         shift
         ;;
      "HUGE" | "huge" )
         HUGE=TRUE
         shift
         ;;
      "WW3" | "ww3")
         WW3=TRUE
         shift
         ;;
      "MIC" | "mic")
         MIC=TRUE
         shift
         ;;
      "CHOU" | "chou")
         CHOU=TRUE
         shift
         ;;
      "RRTMG" | "rrtmg")
         RRTMG=TRUE
         shift
         ;;
      "RRTMG_SW" | "rrtmg_sw")
         RRTMG_SW=TRUE
         shift
         ;;
      "RRTMG_LW" | "rrtmg_lw")
         RRTMG_LW=TRUE
         shift
         ;;
      "NOHIS" | "nohis")
         NOHIS=TRUE
         shift
         ;;
      "HIS" | "his" | "HISTORY" | "history" )
         HISTORY=TRUE
         shift
         ;;
      "POL" | "pol" | "POLICE" | "police" )
         POLICE=TRUE
         shift
         ;;
      "LINK" | "link" )
         LINK=TRUE
         shift
         ;;
      "DAS" | "das" )
         DAS=TRUE
         shift
         ;;
      "BENCH" | "bench" )
         BENCH=TRUE
         shift
         ;;
      "NOCHECK" | "nocheck" )
         NOCHECK=TRUE
         shift
         ;;
      "REPLAY" | "replay" )
         REPLAY=TRUE
         shift
         ;;
      "BRO" | "bro" )
         BRO=TRUE
         shift
         ;;
      "BROELE" | "broele" )
         BROELE=TRUE
         shift
         ;;
      "PGI" | "pgi" )
         PGI=TRUE
         shift
         ;;
      "HEMCO" | "hemco" )
         HEMCO=TRUE
         shift
         ;;
      "SATSIM" | "satsim" )
         SATSIM=TRUE
         shift
         ;;
      "IOS" | "ios" )
         IOS=TRUE
         shift
         ;;
      -h | --help)
         usage
         exit 0
         ;;
      *)
         echo "Unknown option: $1"
         echo ""
         usage
         exit 1
         ;;
   esac
done

if [[ $OPSB == TRUE ]] 
then
   BENCH=TRUE
   MINMAX=TRUE
   MEM=TRUE
   FIVEDAY=TRUE
fi

if [[ $OPSBIO == TRUE ]] 
then
   BENCH=TRUE
   MINMAX=TRUE
   MEM=TRUE
   FIVEDAY=TRUE
fi

if [[ $OPSPORT == TRUE ]] 
then
   BENCH=TRUE
   MINMAX=TRUE
   MEM=TRUE
   FIVEDAY=TRUE
   HUGE=TRUE
fi

if [[ $USEG40 == TRUE ]] 
then
   echo "Using Ganymed-4_0 directories"
   BCDIRNAME="G40"
elif [[ $USEH50 == TRUE ]] 
then
   echo "Using Heracles-5_0 directories"
   BCDIRNAME="H50"
else
   echo "Using Icarus-1_0 directories"
   BCDIRNAME="I10"
fi

if [[ $SIXHOUR == TRUE ]] 
then
   echo "Making six-hour experiment with "
elif [[ $TWODAY == TRUE ]]
then
   echo "Making two-day experiment with "
elif [[ $ZERODAY == TRUE ]]
then
   echo "Making zero-day experiment with "
elif [[ $TEN == TRUE ]]
then
   echo "Making ten-day experiment with "
else
   echo "Making one-day experiment "
fi

if [[ $NC4 == TRUE && $BIN == TRUE ]]
then
   echo "You can't have both NC4 and BIN set to true"
   exit 9
fi

if [[ $USEG40 == TRUE ]]; then echo "     USEG40: $USEG40"; fi
if [[ $USEH50 == TRUE ]]; then echo "     USEH50: $USEH50"; fi
if [[ $OPSB == TRUE ]]; then echo "       OPSB: $OPSB"; fi
if [[ $OPSBIO == TRUE ]]; then echo "     OPSBIO: $OPSBIO"; fi
if [[ $OPSPORT == TRUE ]]; then echo "    OPSPORT: $OPSPORT"; fi
if [[ $SIXHOUR == TRUE ]]; then echo "        6HR: $SIXHOUR"; fi
if [[ $ZERODAY == TRUE ]]; then echo "        0DY: $ZERODAY"; fi
if [[ $TWODAY == TRUE ]]; then echo "        2DY: $TWODAY"; fi
if [[ $FIVEDAY == TRUE ]]; then echo "        5DY: $FIVEDAY"; fi
if [[ $TENDAY == TRUE ]]; then echo "       10DY: $TENDAY"; fi
if [[ $ONEMO == TRUE ]]; then echo "        1MO: $ONEMO"; fi
if [[ $TWOMO == TRUE ]]; then echo "        2MO: $TWOMO"; fi
if [[ $NC4 == TRUE ]]; then echo "        NC4: $NC4"; fi
if [[ $BIN == TRUE ]]; then echo "        BIN: $BIN"; fi
if [[ $MEM == TRUE ]]; then echo "        MEM: $MEM"; fi
if [[ $MINMAX == TRUE ]]; then echo "     MINMAX: $MINMAX"; fi
if [[ $TINY == TRUE ]]; then echo "       TINY: $TINY"; fi
if [[ $HUGE == TRUE ]]; then echo "       HUGE: $HUGE"; fi
if [[ $WW3 == TRUE ]]; then echo "        WW3: $WW3"; fi
if [[ $MIC == TRUE ]]; then echo "        MIC: $MIC"; fi
if [[ $CHOU == TRUE ]]; then echo "        CHOU: $CHOU"; fi
if [[ $RRTMG == TRUE ]]; then echo "      RRTMG: $RRTMG"; fi
if [[ $RRTMG_SW == TRUE ]]; then echo "   RRTMG_SW: $RRTMG_SW"; fi
if [[ $RRTMG_LW == TRUE ]]; then echo "   RRTMG_LW: $RRTMG_LW"; fi
if [[ $NOHIS == TRUE ]]; then echo "      NOHIS: $NOHIS"; fi
if [[ $HISTORY == TRUE ]]; then echo "    HISTORY: $HISTORY"; fi
if [[ $POLICE == TRUE ]]; then echo "     POLICE: $POLICE"; fi
if [[ $LINK == TRUE ]]; then echo "       LINK: $LINK"; fi
if [[ $DAS == TRUE ]]; then echo "        DAS: $DAS"; fi
if [[ $BENCH == TRUE ]]; then echo "      BENCH: $BENCH"; fi
if [[ $NOCHECK == TRUE ]]; then echo "      NOCHECK: $NOCHECK"; fi
if [[ $REPLAY == TRUE ]]; then echo "     REPLAY: $REPLAY"; fi
if [[ $BRO == TRUE ]]; then echo "        BRO: $BRO"; fi
if [[ $BROELE == TRUE ]]; then echo "        BROELE: $BROELE"; fi
if [[ $PGI == TRUE ]]; then echo "        PGI: $PGI"; fi
if [[ $HEMCO == TRUE ]]; then echo "      HEMCO: $HEMCO"; fi
if [[ $SATSIM == TRUE ]]; then echo "     SATSIM: $SATSIM"; fi
if [[ $IOS == TRUE ]]; then echo "     IOS: $IOS"; fi
echo ""

# -------------------
# Locate where we are
# -------------------

NODENAME=$(uname -n)

if [[ $NODENAME == discover* || $NODENAME == dali* || $NODENAME == warp* || $NODENAME == borg* ]]
then
   SITE=NCCS

   COLORDIFF=/home/mathomp4/bin/colordiff
   UTIL_DIR=/discover/nobackup/mathomp4
   PBZIP2=/home/mathomp4/bin/pbzip2

elif [[ $NODENAME == pfe* || $NODENAME == r[0-9]*i[0-9]*n[0-9]* || $NODENAME == bridge* || $NODENAME == maia* ]]
then
   SITE=NAS

   COLORDIFF=/nobackup/gmao_SIteam/Utilities/bin/colordiff
   UTIL_DIR=/nobackup/gmao_SIteam/ModelData
   PBZIP2=/usr/bin/pbzip2

else
   SITE=DESKTOP

   if [[ $ARCH == Darwin ]]
   then
      COLORDIFF=/usr/bin/diff
      UTIL_DIR=/home/aborle1
      PBZIP2=/bin/bzip2
   else
      COLORDIFF=/usr/bin/diff
      UTIL_DIR=/home/aborle1
      PBZIP2=/bin/bzip2
   fi

fi

MIN_DIR=$UTIL_DIR/TinyBCs-$BCDIRNAME
MIN_BCS_DIR=$MIN_DIR/bcs
MIN_SST_DIR=$MIN_DIR/sst
MIN_CHM_DIR=$MIN_DIR/chem
MIN_MIE_DIR=$MIN_DIR/chem/g5chem/x
MIN_AERO_DIR=$MIN_CHM_DIR/g5chem/L72/aero_clm
MIN_RESTART_DIR=$MIN_DIR/rs

HUGE_DIR=$UTIL_DIR/HugeBCs-$BCDIRNAME
HUGE_BCS_DIR=$HUGE_DIR/bcs
HUGE_SST_DIR=$HUGE_DIR/sst
HUGE_CHM_DIR=$HUGE_DIR/chem
HUGE_MIE_DIR=$MIN_DIR/chem/g5chem/x
HUGE_AERO_DIR=$HUGE_CHM_DIR/g5chem/L72/aero_clm
HUGE_RESTART_DIR=$HUGE_DIR/rs

RESTARTS_H10_DIR=Restarts-H10
RESTARTS_I10_DIR=Restarts-I10

# ----------------------------------------
# Replay only works at NCCS. Die otherwise
# ----------------------------------------

#if [[ $REPLAY == TRUE && $SITE != NCCS ]]
#then
   #echo "Detected site: $SITE and REPLAY: $REPLAY"
   #echo "REPLAY only works at NCCS"
   #exit 400
#fi

# ------------------------------------------
# Broadwell only works at NAS. Die otherwise
# ------------------------------------------

if [[ $BRO == TRUE && $SITE != NAS ]]
then
   echo "Detected site: $SITE and BRO: $BRO"
   echo "BRO only works at NAS"
   exit 401
fi

if [[ $BROELE == TRUE && $SITE != NAS ]]
then
   echo "Detected site: $SITE and BROELE: $BROELE"
   echo "BROELE only works at NAS"
   exit 402
fi

# ---------------
# Local Functions
# ---------------

restore_save ()
{
   if [ -e $1.save ]
   then
      echo "Restoring $1.save to $1..."
      mv $1.save $1
   fi
}

copy_save ()
{
   if [ ! -e $1.save ]
   then
      echo "Copying $1 to $1.save..."
      cp $1 $1.save
   fi
}

print_changes ()
{
   DIFF=$(diff "$1.save" "$1")
   if [ $? -ne 0 ]
   then
      echo "Changes made to $1:"
      $COLORDIFF $1.save $1
   fi
   echo
}

convert_rrtmg_none ()
{
   AERODIR=$UTIL_DIR/AerosolTables-$BCDIRNAME/ChouS-ChouI

   if [[ $TINY == TRUE ]]
   then
      AERODIR=$MIN_MIE_DIR/ChouS-ChouI
   elif [[ $HUGE == TRUE ]]
   then
      AERODIR=$HUGE_MIE_DIR/ChouS-ChouI
   fi

   $SED -r -e "/^USE_RRTMG_IRRAD/d" AGCM.rc
   $SED -r -e "/^USE_RRTMG_SORAD/d" AGCM.rc

   $SED -r -e "/^DU_OPTICS:/ s#ExtData/.*/x/opticsBands_DU.v14_2.*nc#$AERODIR/opticsBands_DU.ChouS-ChouI.v14_2.nc#" \
           -e "/^DU_OPTICS:/ s#ExtData/.*/x/opticsBands_DU.v15_3.*nc#$AERODIR/opticsBands_DU.ChouS-ChouI.v15_3.nc#" \
           -e "/^SS_OPTICS:/ s#ExtData/.*/x/opticsBands_SS.v3_3.*nc#$AERODIR/opticsBands_SS.ChouS-ChouI.v3_3.nc#" \
           -e "/^SU_OPTICS:/ s#ExtData/.*/x/opticsBands_SU.v1_3.*nc#$AERODIR/opticsBands_SU.ChouS-ChouI.v1_3.nc#" \
           -e "/^OC_OPTICS:/ s#ExtData/.*/x/opticsBands_OC.v1_3.*nc#$AERODIR/opticsBands_OC.ChouS-ChouI.v1_3.nc#" \
           -e "/^BC_OPTICS:/ s#ExtData/.*/x/opticsBands_BC.v1_3.*nc#$AERODIR/opticsBands_BC.ChouS-ChouI.v1_3.nc#" \
           -e "/^NI_OPTICS:/ s#ExtData/.*/x/opticsBands_NI.v1_5.*nc#$AERODIR/opticsBands_NI.ChouS-ChouI.v1_5.nc#" \
           -e "/^NI_OPTICS:/ s#ExtData/.*/x/opticsBands_NI.v2_5.*nc#$AERODIR/opticsBands_NI.ChouS-ChouI.v2_5.nc#" \
           -e "/^BRC_OPTICS:/ s#ExtData/.*/x/opticsBands_BRC.v1_5.*nc#$AERODIR/opticsBands_BRC.ChouS-RRTMGI.v1_5.nc#" \
           -e "/^BRC_OPTICS:/ s#ExtData/.*/x/opticsBands_BRC.v2_5.*nc#$AERODIR/opticsBands_BRC.ChouS-RRTMGI.v2_5.nc#" \
           -e "/^NUM_BANDS:/ s/[0-9]{2}/18/" AGCM.rc
}

convert_rrtmg_lw ()
{
   AERODIR=$UTIL_DIR/AerosolTables-$BCDIRNAME/ChouS-RRTMGI

   if [[ $TINY == TRUE ]]
   then
      AERODIR=$MIN_MIE_DIR/ChouS-RRTMGI
   elif [[ $HUGE == TRUE ]]
   then
      AERODIR=$HUGE_MIE_DIR/ChouS-RRTMGI
   fi

   $SED -r -e "/^USE_RRTMG_IRRAD/d" AGCM.rc
   $SED -r -e "/^USE_RRTMG_SORAD/d" AGCM.rc

   $SED -r -e "/^DU_OPTICS:/ s#ExtData/.*/x/opticsBands_DU.v14_2.*nc#$AERODIR/opticsBands_DU.ChouS-RRTMGI.v14_2.nc#" \
           -e "/^DU_OPTICS:/ s#ExtData/.*/x/opticsBands_DU.v15_3.*nc#$AERODIR/opticsBands_DU.ChouS-RRTMGI.v15_3.nc#" \
           -e "/^SS_OPTICS:/ s#ExtData/.*/x/opticsBands_SS.v3_3.*nc#$AERODIR/opticsBands_SS.ChouS-RRTMGI.v3_3.nc#" \
           -e "/^SU_OPTICS:/ s#ExtData/.*/x/opticsBands_SU.v1_3.*nc#$AERODIR/opticsBands_SU.ChouS-RRTMGI.v1_3.nc#" \
           -e "/^OC_OPTICS:/ s#ExtData/.*/x/opticsBands_OC.v1_3.*nc#$AERODIR/opticsBands_OC.ChouS-RRTMGI.v1_3.nc#" \
           -e "/^BC_OPTICS:/ s#ExtData/.*/x/opticsBands_BC.v1_3.*nc#$AERODIR/opticsBands_BC.ChouS-RRTMGI.v1_3.nc#" \
           -e "/^NI_OPTICS:/ s#ExtData/.*/x/opticsBands_NI.v1_5.*nc#$AERODIR/opticsBands_NI.ChouS-RRTMGI.v1_5.nc#" \
           -e "/^NI_OPTICS:/ s#ExtData/.*/x/opticsBands_NI.v2_5.nc#$AERODIR/opticsBands_NI.ChouS-RRTMGI.v2_5.nc#" \
           -e "/^BRC_OPTICS:/ s#ExtData/.*/x/opticsBands_BRC.v1_5.*nc#$AERODIR/opticsBands_BRC.ChouS-RRTMGI.v1_5.nc#" \
           -e "/^BRC_OPTICS:/ s#ExtData/.*/x/opticsBands_BRC.v2_5.*nc#$AERODIR/opticsBands_BRC.ChouS-RRTMGI.v2_5.nc#" \
           -e "/^NUM_BANDS:/ s/[0-9]{2}/24/" AGCM.rc

   echo "USE_RRTMG_IRRAD: 1.0" >> AGCM.rc
}

convert_rrtmg_sw ()
{
   AERODIR=$UTIL_DIR/AerosolTables-$BCDIRNAME/RRTMGS-ChouI

   if [[ $TINY == TRUE ]]
   then
      AERODIR=$MIN_MIE_DIR/RRTMGS-ChouI
   elif [[ $HUGE == TRUE ]]
   then
      AERODIR=$HUGE_MIE_DIR/RRTMGS-ChouI
   fi

   $SED -r -e "/^USE_RRTMG_IRRAD/d" AGCM.rc
   $SED -r -e "/^USE_RRTMG_SORAD/d" AGCM.rc

   $SED -r -e "/^DU_OPTICS:/ s#ExtData/.*/x/opticsBands_DU.v14_2.*nc#$AERODIR/opticsBands_DU.RRTMGS-ChouI.v14_2.nc#" \
           -e "/^DU_OPTICS:/ s#ExtData/.*/x/opticsBands_DU.v15_3.*nc#$AERODIR/opticsBands_DU.RRTMGS-ChouI.v15_3.nc#" \
           -e "/^SS_OPTICS:/ s#ExtData/.*/x/opticsBands_SS.v3_3.*nc#$AERODIR/opticsBands_SS.RRTMGS-ChouI.v3_3.nc#" \
           -e "/^SU_OPTICS:/ s#ExtData/.*/x/opticsBands_SU.v1_3.*nc#$AERODIR/opticsBands_SU.RRTMGS-ChouI.v1_3.nc#" \
           -e "/^OC_OPTICS:/ s#ExtData/.*/x/opticsBands_OC.v1_3.*nc#$AERODIR/opticsBands_OC.RRTMGS-ChouI.v1_3.nc#" \
           -e "/^BC_OPTICS:/ s#ExtData/.*/x/opticsBands_BC.v1_3.*nc#$AERODIR/opticsBands_BC.RRTMGS-ChouI.v1_3.nc#" \
           -e "/^NI_OPTICS:/ s#ExtData/.*/x/opticsBands_NI.v1_5.*nc#$AERODIR/opticsBands_NI.RRTMGS-ChouI.v1_5.nc#" \
           -e "/^NI_OPTICS:/ s#ExtData/.*/x/opticsBands_NI.v2_5.*nc#$AERODIR/opticsBands_NI.RRTMGS-ChouI.v2_5.nc#" \
           -e "/^BRC_OPTICS:/ s#ExtData/.*/x/opticsBands_BRC.v1_5.*nc#$AERODIR/opticsBands_BRC.RRTMGS-ChouI.v1_5.nc#" \
           -e "/^BRC_OPTICS:/ s#ExtData/.*/x/opticsBands_BRC.v2_5.*nc#$AERODIR/opticsBands_BRC.RRTMGS-ChouI.v2_5.nc#" \
           -e "/^NUM_BANDS:/ s/[0-9]{2}/24/" AGCM.rc

   echo "USE_RRTMG_SORAD: 1.0" >> AGCM.rc
}

convert_rrtmg_swlw ()
{
   AERODIR=$UTIL_DIR/AerosolTables-$BCDIRNAME/RRTMGS-RRTMGI

   if [[ $TINY == TRUE ]]
   then
      AERODIR=$MIN_MIE_DIR/RRTMGS-RRTMGI
   elif [[ $HUGE == TRUE ]]
   then
      AERODIR=$HUGE_MIE_DIR/RRTMGS-RRTMGI
   fi

   $SED -r -e "/^USE_RRTMG_IRRAD/d" AGCM.rc
   $SED -r -e "/^USE_RRTMG_SORAD/d" AGCM.rc

   $SED -r -e "/^DU_OPTICS:/ s#ExtData/.*/x/opticsBands_DU.v14_2.*nc#$AERODIR/opticsBands_DU.RRTMGS-RRTMGI.v14_2.nc#" \
           -e "/^DU_OPTICS:/ s#ExtData/.*/x/opticsBands_DU.v15_3.*nc#$AERODIR/opticsBands_DU.RRTMGS-RRTMGI.v15_3.nc#" \
           -e "/^SS_OPTICS:/ s#ExtData/.*/x/opticsBands_SS.v3_3.*nc#$AERODIR/opticsBands_SS.RRTMGS-RRTMGI.v3_3.nc#" \
           -e "/^SU_OPTICS:/ s#ExtData/.*/x/opticsBands_SU.v1_3.*nc#$AERODIR/opticsBands_SU.RRTMGS-RRTMGI.v1_3.nc#" \
           -e "/^OC_OPTICS:/ s#ExtData/.*/x/opticsBands_OC.v1_3.*nc#$AERODIR/opticsBands_OC.RRTMGS-RRTMGI.v1_3.nc#" \
           -e "/^BC_OPTICS:/ s#ExtData/.*/x/opticsBands_BC.v1_3.*nc#$AERODIR/opticsBands_BC.RRTMGS-RRTMGI.v1_3.nc#" \
           -e "/^NI_OPTICS:/ s#ExtData/.*/x/opticsBands_NI.v1_5.*nc#$AERODIR/opticsBands_NI.RRTMGS-RRTMGI.v1_5.nc#" \
           -e "/^NI_OPTICS:/ s#ExtData/.*/x/opticsBands_NI.v2_5.*nc#$AERODIR/opticsBands_NI.RRTMGS-RRTMGI.v2_5.nc#" \
           -e "/^BRC_OPTICS:/ s#ExtData/.*/x/opticsBands_BRC.v1_5.*nc#$AERODIR/opticsBands_BRC.RRTMGS-RRTMGI.v1_5.nc#" \
           -e "/^BRC_OPTICS:/ s#ExtData/.*/x/opticsBands_BRC.v2_5.*nc#$AERODIR/opticsBands_BRC.RRTMGS-RRTMGI.v2_5.nc#" \
           -e "/^NUM_BANDS:/ s/[0-9]{2}/30/" AGCM.rc

   echo "USE_RRTMG_IRRAD: 1.0" >> AGCM.rc
   echo "USE_RRTMG_SORAD: 1.0" >> AGCM.rc
}

# ------------------------------------
# Restore to original all edited files
# ------------------------------------

restore_save "AGCM.rc"
copy_save "AGCM.rc"

restore_save "CAP.rc"
copy_save "CAP.rc"

restore_save "HISTORY.rc"
copy_save "HISTORY.rc"

restore_save "gcm_run.j"
copy_save "gcm_run.j"

restore_save "regress/gcm_regress.j"
copy_save "regress/gcm_regress.j"

#restore_save "RC/GOCARTdata_ExtData.rc"
#copy_save "RC/GOCARTdata_ExtData.rc"

restore_save "RC/GEOS_ChemGridComp.rc"
copy_save "RC/GEOS_ChemGridComp.rc"

restore_save "RC/GAAS_GridComp.rc"
copy_save "RC/GAAS_GridComp.rc"

# -----------------------------
# Detect Atmospheric Resolution
# -----------------------------

AGCM_IM=$(grep "^ \+AGCM_IM:" AGCM.rc | awk '{print $2}')

case $AGCM_IM in
   12)
   ATMOS_RES="c12"
   ;;
   24)
   ATMOS_RES="c24"
   ;;
   48)
   ATMOS_RES="c48"
   ;;
   90)
   ATMOS_RES="c90"
   ;;
   180)
   ATMOS_RES="c180"
   ;;
   360)
   ATMOS_RES="c360"
   ;;
   720)
   ATMOS_RES="c720"
   ;;
   1000)
   ATMOS_RES="c1000"
   ;;
   1440)
   ATMOS_RES="c1440"
   ;;
   2880)
   ATMOS_RES="c2880"
   ;;
   72)
   ATMOS_RES="72x46"
   ;;
   144)
   ATMOS_RES="144x91"
   ;;
   288)
   ATMOS_RES="288x181"
   ;;
   576)
   ATMOS_RES="576x361"
   ;;
   1152)
   ATMOS_RES="1152x721"
   ;;
   *)
   ATMOS_RES="UNKNOWN"
   echo "$ATMOS_RES atmospheric resolution found!"
   ;;
esac

# --------------------------------
# Detect Number of Pressure Levels
# --------------------------------

AGCM_LM=$(grep "^ \+AGCM_LM:" AGCM.rc | awk '{print $2}')

case $AGCM_LM in
   72)
   ATMOS_LEVS="72"
   ;;
   132)
   ATMOS_LEVS="132"
   ;;
   137)
   ATMOS_LEVS="137"
   ;;
   144)
   ATMOS_LEVS="144"
   ;;
   *)
   ATMOS_LEVS="UNKNOWN"
   echo "Unknown number of atmospheric levels detected: $AGCM_LM"
   exit 338
   ;;
esac

# For now, append L<levs) if we are running other than 72
# -------------------------------------------------------

if [[ $ATMOS_LEVS == 72 ]]
then
   ATMOS_RES_LEVS=$ATMOS_RES
else
   ATMOS_RES_LEVS=$ATMOS_RES-L$ATMOS_LEVS
fi

# -----------------------
# Detect Ocean Resolution
# -----------------------

OGCM_GRIDNAME=$(grep "^OGCM_GRIDNAME:" AGCM.rc | awk '{print $2}')
OGCM_GRID_TYPE=$(echo ${OGCM_GRIDNAME#*-})

case $OGCM_GRID_TYPE in
   CF)
   OCEAN_RES="Ostia-CS"
   ;;
   DE)
   OGCM_IM=$(grep "^ \+OGCM_IM:" AGCM.rc | awk '{print $2}')

   case $OGCM_IM in
      360)
      OCEAN_RES="Reynolds"
      ;;
      1440)
      OCEAN_RES="MERRA-2"
      ;;
      2880)
      OCEAN_RES="Ostia"
      ;;
      *)
      OCEAN_RES="UNKNOWN"
      echo "Unknown $OCEAN_RES ocean latlon resolution found!"
      ;;
   esac
   ;;
   *)
   OCEAN_RES="UNKNOWN"
   echo "Unknown $OGCM_GRID_TYPE found!"
   ;;
esac

# --------------------
# Detect Restart Types
# --------------------

DYN_RESTART_TYPE=$(grep "^DYN_INTERNAL_RESTART_TYPE:" AGCM.rc | awk '{print $2}')

case $DYN_RESTART_TYPE in
   binary)
   RESTART_TYPE="binary"
   ;;
   pbinary)
   RESTART_TYPE="binary"
   ;;
   pnc4)
   RESTART_TYPE="nc4"
   ;;
   *)
   echo "DYN_INTERNAL_RESTART_TYPE not found. Assuming NC4"
   RESTART_TYPE="nc4"
   ;;
esac

# -----------------------------------------
# Do we want to convert to NC4 or to binary
# -----------------------------------------

if [[ $NC4 == TRUE && $RESTART_TYPE == "binary" ]]
then
   echo "NC4 requested and binary restart found. Converting to NC4"

   #$SED -e "/_rst$/ s/_rst/_rst.nc4/" \
        #-e "/_checkpoint$/ s/_checkpoint/_checkpoint.nc4/" AGCM.rc

   $SED -e "/.*_TYPE: \+pbinary$/ s/pbinary/pnc4/" \
        -e "/.*_TYPE: \+binary$/ s/binary/pnc4/" AGCM.rc

   # The above clobbers VEGDYN. Undo.
   $SED -e "/VEGDYN/ s/pnc4/binary/" AGCM.rc

   RESTART_TYPE="nc4"

elif [[ $BIN == TRUE && $RESTART_TYPE == "nc4" ]]
then
   echo "Binary requested and NC4 restart found. Converting to binary"

   #$SED -e "/_rst$/ s/_rst/_rst.nc4/" \
        #-e "/_checkpoint$/ s/_checkpoint/_checkpoint.nc4/" AGCM.rc

   $SED -e "/.*_TYPE: \+pnc4$/ s/pnc4/binary/" AGCM.rc

   # The above clobbers VEGDYN. Undo.
   #$SED -e "/VEGDYN/ s/pnc4/binary/" AGCM.rc

   RESTART_TYPE="binary"
fi


# ----------------------------------
# Do our restart files have suffixes
# ----------------------------------

FVNAME=$(grep "^DYN_INTERNAL_RESTART_FILE:" AGCM.rc | awk '{print $2}')
EXTENSION="${FVNAME##*.}"

# --------------------------
# Detect Boundary Conditions
# --------------------------

BCSDIR=$(grep "^setenv BCSDIR" gcm_run.j | awk '{print $3}')

#echo $BCSDIR

USING_4_0_BCS=$(echo $BCSDIR | grep -ow "Ganymed-4_0" )
USING_HNL_BCS=$(echo $BCSDIR | grep -ow "Heracles-NL" )
USING_Icarus_BCS=$(echo $BCSDIR | grep -ow "Icarus" )

#echo $USING_4_0_BCS

if [[ $USING_Icarus_BCS == "Icarus" ]]
then
   if [[ $TINY == TRUE ]]
   then
      RESTARTS_DIR=$MIN_RESTART_DIR
   elif [[ $HUGE == TRUE ]]
   then
      RESTARTS_DIR=$HUGE_RESTART_DIR
   else
      RESTARTS_DIR=$UTIL_DIR/$RESTARTS_I10_DIR
   fi
elif [[ $USING_4_0_BCS == "Ganymed-4_0" ]]
then
   if [[ $TINY == TRUE ]]
   then
      RESTARTS_DIR=$MIN_RESTART_DIR
   elif [[ $HUGE == TRUE ]]
   then
      RESTARTS_DIR=$HUGE_RESTART_DIR
   else
      RESTARTS_DIR=$UTIL_DIR/$RESTARTS_H10_DIR
   fi
elif [[ $USING_HNL_BCS == "Heracles-NL" ]]
then
   RESTARTS_DIR=$UTIL_DIR/$RESTARTS_HNL_DIR
else
   echo "You seem to be using an unknown BCSDIR:"
   echo "   $BCSDIR"
   echo
   echo "This script can handle Ganymed-4_0."
   exit
fi

# -------------
# Link Restarts
# -------------

if [[ ! -e fvcore_internal_rst ]]
then
   if [[ ! -d $RESTARTS_DIR/$RESTART_TYPE/$OCEAN_RES/$ATMOS_RES_LEVS ]]
   then
      echo "FAILURE!"
      echo "Restarts of type $RESTART_TYPE for resolution $ATMOS_RES using $ATMOS_LEVS levels on ocean $OCEAN_RES do not exist in $RESTARTS_DIR"
      exit 2
   else
      echo "Linking $RESTART_TYPE restarts for resolution $ATMOS_RES using $ATMOS_LEVS levels on ocean $OCEAN_RES from $RESTARTS_DIR..."
      if [[ ${EXTENSION} == ${FVNAME} ]]
      then
         echo "Restarts have no EXTENSION..."
         ln -sv $RESTARTS_DIR/$RESTART_TYPE/$OCEAN_RES/$ATMOS_RES_LEVS/*_rst .
      else
         echo "Restarts have EXTENSION: $EXTENSION"
         if [[ -e $RESTARTS_DIR/$RESTART_TYPE/$OCEAN_RES/$ATMOS_RES_LEVS/fvcore_internal_rst.$EXTENSION ]]
         then
            ln -sv $RESTARTS_DIR/$RESTART_TYPE/$OCEAN_RES/$ATMOS_RES_LEVS/*_rst.$EXTENSION .
         else
            for file in $RESTARTS_DIR/$RESTART_TYPE/$OCEAN_RES/$ATMOS_RES_LEVS/*_rst
            do
               filename=$(basename $file)
               ln -sv $file ./$filename.$EXTENSION
            done
         fi
      fi
      if [[ ! -e cap_restart ]]
      then
         cp $RESTARTS_DIR/$RESTART_TYPE/$OCEAN_RES/$ATMOS_RES_LEVS/cap_restart .
         if [[ ! -w cap_restart ]]
         then
            echo "cap_restart seems to be read only. For safety's sake, we make it writable"
            chmod -v u+w cap_restart
         fi
      else
         echo "cap_restart already exists. Not copying."
      fi
   fi
else
   echo "Found fvcore_internal_rst. Assuming you have needed restarts!"
fi

if [[ $WW3 == TRUE ]]
then
   echo -n "Linking WW3 mod_def.ww3 file for $OCEAN_RES"
   if [[ $OCEAN_RES == "Reynolds" ]] 
   then
      WW3_DIR=$UTIL_DIR/GridGen-v2.1/geos_1deg_latlon_grid_dateline_edge_poleline_edge
      echo " from $WW3_DIR"
      MOD_FILE=mod_def.ww3.CFL_for_30m
      ln -s $WW3_DIR/$MOD_FILE .
      ln -s $MOD_FILE mod_def.ww3
   elif [[ $OCEAN_RES == "Ostia" ]]
   then
      WW3_DIR=$UTIL_DIR/GridGen-v2.1/ostia_eighth_latlon_grid_dateline_edge_poleline_edge
      echo " from $WW3_DIR"
      MOD_FILE=mod_def.ww3.150_50_75_15
      ln -s $WW3_DIR/$MOD_FILE .
      ln -s $MOD_FILE mod_def.ww3
   elif [[ $OCEAN_RES == "MERRA-2" ]]
   then
      WW3_DIR=$UTIL_DIR/GridGen-v2.1/merra2_quart_deg_latlon_grid_dateline_edge_poleline_edge
      echo " from $WW3_DIR"
      MOD_FILE=mod_def.ww3.300_100_150_30
      ln -s $WW3_DIR/$MOD_FILE .
      ln -s $MOD_FILE mod_def.ww3
   else
      echo ""
      echo "ERROR: No WW3 mod_def.ww3 available for $OCEAN_RES"
      exit 2
   fi
fi


# ------
# CAP.rc
# ------

if [[ $SIXHOUR == TRUE ]]
then
   $SED -e '/^JOB_SGMT:/ s/000000[0-9][0-9] 000000/00000000 060000/' CAP.rc
elif [[ $TWODAY == TRUE ]]
then
   $SED -e '/^JOB_SGMT:/ s/000000[0-9][0-9]/00000002/' CAP.rc
elif [[ $ZERODAY == TRUE ]]
then
   $SED -e '/^JOB_SGMT:/ s/000000[0-9][0-9]/00000000/' CAP.rc
elif [[ $FIVEDAY == TRUE ]]
then
   $SED -e '/^JOB_SGMT:/ s/000000[0-9][0-9]/00000005/' CAP.rc
elif [[ $TENDAY == TRUE ]]
then
   $SED -e '/^JOB_SGMT:/ s/000000[0-9][0-9]/00000010/' CAP.rc
else
   $SED -e '/^JOB_SGMT:/ s/000000[0-9][0-9]/00000001/' CAP.rc
fi

$SED -e '/^NUM_SGMT:/ s/[0-9][0-9]*/1/' \
     -e '/^MAPL_ENABLE_MEMUTILS:/ s/NO/NO/' \
     -e '/^MAPL_ENABLE_TIMERS:/ s/NO/YES/' CAP.rc

if [[ $MEM == TRUE ]]
then
   $SED -e '/^MAPL_ENABLE_MEMUTILS:/ s/NO/YES/' CAP.rc
fi

if [[ $MINMAX == TRUE ]]
then
   $SED -e '/^MAPL_ENABLE_MEMUTILS:/ a MAPL_TIMER_MODE: MINMAX' CAP.rc
fi

if [[ $OPSB == TRUE || $OPSBIO == TRUE || $OPSPORT == TRUE ]]
then
   $SED -e '/^USE_SHMEM:/ s/0/1/' CAP.rc
fi

if [[ $OPSBIO == TRUE ]]
then
   $SED -e '/^USE_SHMEM:/ a USE_IOSERVER: 1' CAP.rc
fi

if [[ $IOS == TRUE ]]
then
   $SED -e '/^USE_SHMEM:/ a USE_IOSERVER: 1' CAP.rc
fi

$SED -e '/^CoresPerNode:/ d' CAP.rc

print_changes "CAP.rc"

# ----------
# HISTORY.rc
# ----------

if [[ $HISTORY == TRUE ]]
then

   # This command should add a # to all lines that have geosgcm
   # or tavg with spaces at the beginning between COLLECTIONS and ::

   $SED -e "/^COLLECTIONS:/,/.*  ::/ {
                                     /^ .*geosgcm/ s/ /#/
                                     /^ .*tavg/    s/ /#/}" HISTORY.rc

fi

if [[ $NOHIS == TRUE ]]
then

   # This command deletes all lines that have geosgcm
   # or tavg with spaces (and start with #)
   # at the beginning between COLLECTIONS and ::

   $SED -e "/^COLLECTIONS:/,/.*  ::/ {
                                     /^ .*geosgcm/ d
                                     /^# .*geosgcm/ d
                                     /^ .*tavg/    d}" HISTORY.rc

   # Now we take care of the collection that is on the same line
   # as COLLECTIONS is

   $SED -e "/^COLLECTIONS:/ s/\(COLLECTIONS:\).*/\1/" HISTORY.rc
fi


if [[ $TINY == TRUE ]]
then

   echo "Since we turn off TR, tracer collection cannot run"

   $SED -e "/ *'geosgcm_tracer'/ s/ /#/" HISTORY.rc
fi

if [[ $SATSIM == TRUE ]]
then

   echo "Enabling ISCCP collection"

   $SED -e "/^#.*'geosgcm_isccp'/ s/#//" HISTORY.rc
fi

if [[ $OPSB == TRUE || $OPSBIO == TRUE ]]
then
   cp -v $SCRIPTDIR/HISTORY.rc.f516_fp_20170216_21z.for20150414 HISTORY.rc
   echo
   echo "HISTORY.rc has been replaced with OPS history!!!"
   echo
   if [[ $OPSBIO == TRUE ]]
   then
      echo "Enabling CFIOasync"

      $SED -e "s/CFIO/CFIOasync/" HISTORY.rc
   fi
else
   print_changes "HISTORY.rc"
fi

# ---------
# gcm_run.j
# ---------

$SED -e '/^echo GEOSgcm/ a exit' \
     -e '/^#SBATCH --account/ a #SBATCH --mail-type=ALL' \
     -e '/^#SBATCH -A / a #SBATCH --mail-type=ALL' \
     -e '/^#PBS -W group_list/ a #SBATCH --mail-type=ALL' \
     -e '/numrs == 0/ s/== 0/== 1/ ' gcm_run.j

if [[ $TENDAY != TRUE && $FIVEDAY != TRUE ]]
then

   if (( $AGCM_IM < 180 ))
   then
      $SED -e '/^#PBS -l walltime/ s/=12:00:/=0:15:/ ' \
          -e '/^#PBS -l walltime/ s/=8:00:/=0:15:/ ' \
          -e '/^#SBATCH --time/ s/=12:00:/=0:15:/ ' gcm_run.j
   elif (( $AGCM_IM < 720 ))
   then
      $SED -e '/^#PBS -l walltime/ s/=12:/=1:/ ' \
          -e '/^#PBS -l walltime/ s/=8:/=1:/ ' \
          -e '/^#SBATCH --time/ s/=12:/=1:/ ' gcm_run.j
   else
      $SED -e '/^#PBS -l walltime/ s/=12:/=2:/ ' \
          -e '/^#PBS -l walltime/ s/=8:/=2:/ ' \
          -e '/^#SBATCH --time/ s/=12:/=2:/ ' gcm_run.j
   fi
fi

if [[ $LINK == TRUE ]]
then
   $SED -e '/^  if(-e $EXPDIR\/$rst ) \/bin/ s/cp/ln -s/' gcm_run.j
fi

if [[ $SITE == NAS ]] 
then
   $SED -e '/^#PBS -l walltime/ a #PBS -W umask=0022' \
        -e '/^#PBS -l walltime/ a #PBS -m abe' gcm_run.j

   $SED -e '/RmShmKeys/ s#\$GEOSBIN/RmShmKeys_sshmpi.csh#/u/mathomp4/bin/RmShmKeys_pdsh#' gcm_run.j
fi

if [[ $DAS == TRUE ]]
then
   $SED -e '/^#SBATCH --account/ a #SBATCH --reservation=das' \
        -e '/^#SBATCH --account/ a #SBATCH --qos=das2015' \
        -e '/^#SBATCH -A / a #SBATCH --reservation=das' \
        -e '/^#SBATCH -A/ a #SBATCH --qos=das2015' gcm_run.j
fi

if [[ $BENCH == TRUE ]]
then
   $SED -e '/^#SBATCH --account/ a #SBATCH --partition=preops' \
        -e '/^#SBATCH --account/ a #SBATCH --qos=benchmark' \
        -e '/^#SBATCH -A / a #SBATCH --partition=preops' \
        -e '/^#SBATCH -A/ a #SBATCH --qos=benchmark' gcm_run.j
fi

if [[ $PGI == TRUE ]]
then
   $SED -e '/^#SBATCH --ntasks/ a #SBATCH --ntasks-per-node=12' gcm_run.j
fi

if [[ $BRO == TRUE ]]
then
   $SED -e '/^#PBS -l select/ s/ncpus=24:mpiprocs=24:model=has/ncpus=28:mpiprocs=28:model=bro/' gcm_run.j
fi

if [[ $BROELE == TRUE ]]
then
   $SED -e '/^#PBS -l select/ s/ncpus=24:mpiprocs=24:model=has/ncpus=28:mpiprocs=24:model=bro_ele/' gcm_run.j
fi

if [[ $WW3 == TRUE ]]
then
   echo "Enabling WW3 in gcm_run.j"
   $SED -e '/                             \/bin\/cp \-f/ a\
                             /bin/cp -f $EXPDIR/*.ww3 .' gcm_run.j
fi 

if [[ $POLICE == TRUE ]]
then
   echo "Enabling PoliceMe in gcm_run.j"
   $SED -e '/limit stacksize/ a\
\
if { /home/mathomp4/bin/amibatch } then \
   echo "I am running in batch mode. Executing PoliceMe" \
   mkdir -p policeme \
   /usr/local/other/policeme/policeme.exe -d policeme \
endif ' gcm_run.j
fi

if [[ $MIC == TRUE ]]
then
   echo "Enabling for MIC"
   $SED -e '/limit stacksize/ a \
\
   setenv PATH /opt/intel/mic/bin:${PATH} \
   setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/opt/intel/mic/coi/host-linux-release/lib \
 \
   unsetenv MIC_ENV_PREFIX \
   unsetenv MIC_OMP_NUM_THREADS \
   unsetenv OMP_NUM_THREADS \
   unsetenv OFFLOAD_INIT \
   unsetenv MIC_USE_2MB_BUFFERS \
   unsetenv MIC_KMP_AFFINITY \
   unsetenv MKL_MIC_ENABLE \
   unsetenv OFFLOAD_REPORT \
   unsetenv I_MPI_MIC  \
   unsetenv I_MPI_FABRIC \
   unsetenv I_MPI_DEBUG \
 \
   setenv MIC_ENV_PREFIX MIC \
   setenv MIC_OMP_NUM_THREADS 236 #multiple of 59 4 threads per core \
   setenv OMP_NUM_THREADS 16 \
   setenv OFFLOAD_INIT on_start #good for timing prepares MiC \
   setenv MIC_USE_2MB_BUFFERS 2K \
   setenv MIC_KMP_AFFINITY balanced,granularity=fine \
   setenv MKL_MIC_ENABLE 1 \
   setenv OFFLOAD_REPORT 1 \
   setenv I_MPI_MIC 1  \
   setenv I_MPI_FABRIC shm:ofa \
   setenv I_MPI_DEBUG 5 ' \
   -e '/mpirun_rsh/ s/mpirun_rsh/mpirun_rsh -export/ ' gcm_run.j
fi

if [[ $TINY == TRUE ]]
then
   echo "Using minimal boundary datasets"
   $SED -e "/^setenv BCSDIR/ s#\(setenv BCSDIR\)\(.*\)#\1   ${MIN_BCS_DIR}#" \
        -e "/^setenv SSTDIR/ s#\(setenv SSTDIR\)\(.*\)#\1   ${MIN_SST_DIR}#" \
        -e "/^setenv CHMDIR/ s#\(setenv CHMDIR\)\(.*\)#\1   ${MIN_CHM_DIR}#" \
        -e "/pchem.species/  s#1870-2097#1999-2000#"                         \
        -e "/sst_1971/       s#1971-current#2000#"                           \
        -e "/fraci_1971/     s#1971-current#2000#"                               gcm_run.j
fi

if [[ $HUGE == TRUE ]]
then
   echo "Using maximal boundary datasets"
   $SED -e "/^setenv BCSDIR/ s#\(setenv BCSDIR\)\(.*\)#\1   ${HUGE_BCS_DIR}#" \
        -e "/^setenv SSTDIR/ s#\(setenv SSTDIR\)\(.*\)#\1   ${HUGE_SST_DIR}#" \
        -e "/^setenv CHMDIR/ s#\(setenv CHMDIR\)\(.*\)#\1   ${HUGE_CHM_DIR}#"    gcm_run.j
fi

if [[ $OPSB == TRUE || $OPSPORT == TRUE ]]
then
   $SED -e "/^#SBATCH --constraint=/ s/sp3/hasw/" \
        -e "/^#SBATCH --ntasks=/ s/1536/5400/" \
        -e "/^#SBATCH --ntasks=/ a #SBATCH --ntasks-per-node=27" \
        -e '/^#PBS -l walltime/ s/=12:/=3:/ ' \
        -e '/^#PBS -l walltime/ s/=8:/=3:/ ' \
        -e '/^#PBS -l select/ s/select=64:ncpus=24:mpiprocs=24:model=has/select=200:ncpus=27:mpiprocs=27:model=bro/' gcm_run.j

   $SED -e '/unsetenv PMI_RANK/ a \
 \
  setenv MPI_XPMEM_ENABLED no \
  setenv SUPPRESS_XPMEM_TRIM_THRESH 1 \
  setenv MPI_NUM_MEMORY_REGIONS 0  \
 \
  setenv MPI_DISPLAY_SETTINGS 1 \
  setenv MPI_VERBOSE 1 \
 \
  setenv MPI_COMM_MAX  1024 \
  setenv MPI_GROUP_MAX 1024 \
  setenv MPI_BUFS_PER_PROC 256  \
 \
  setenv MPI_IB_TIMEOUT 23 ' gcm_run.j
fi

if [[ $OPSBIO == TRUE ]]
then
   $SED -e "/^#SBATCH --constraint=/ s/sp3/hasw/" \
        -e "/^#SBATCH --ntasks=/ s/1536/5535/" \
        -e "/^#SBATCH --ntasks=/ a #SBATCH --ntasks-per-node=27" \
        -e '/^#PBS -l walltime/ s/=12:/=3:/ ' \
        -e '/^#PBS -l walltime/ s/=8:/=3:/ ' \
        -e '/^#PBS -l select/ s/select=64:ncpus=24:mpiprocs=24:model=has/select=205:ncpus=27:mpiprocs=27:model=bro/' gcm_run.j
   
   $SED -e '/^$RUN_CMD $NPES/ s/$NPES/5562/ ' gcm_run.j

   $SED -e '/unsetenv PMI_RANK/ a \
 \
  setenv MPI_XPMEM_ENABLED no \
  setenv SUPPRESS_XPMEM_TRIM_THRESH 1 \
  setenv MPI_NUM_MEMORY_REGIONS 0  \
 \
  setenv MPI_DISPLAY_SETTINGS 1 \
  setenv MPI_VERBOSE 1 \
 \
  setenv MPI_COMM_MAX  1024 \
  setenv MPI_GROUP_MAX 1024 \
  setenv MPI_BUFS_PER_PROC 256  \
 \
  setenv MPI_IB_TIMEOUT 23 ' gcm_run.j
fi

print_changes "gcm_run.j"

# -------
# AGCM.rc
# -------

DOING_GOCART=$(grep AERO_PROVIDER AGCM.rc | awk '{print $2}')

FOUND_BOOTSTRAP=$(grep MAPL_ENABLE_BOOTSTRAP AGCM.rc | awk '{print $1}')

#$SED -e "/^NUM_WRITERS:/ s/4/1/" AGCM.rc

if [[ "$FOUND_BOOTSTRAP" == "MAPL_ENABLE_BOOTSTRAP:" ]]
then
   $SED -e "/^MAPL_ENABLE_BOOTSTRAP:/ s/NO/YES/" AGCM.rc
else
   if [[ "$DOING_GOCART" == "GOCART" && ! -e 'gocart_internal_rst' ]]
   then
      echo "Didn't see MAPL_ENABLE_BOOTSTRAP"
      echo "For safety's sake, we bootstrap gocart"
      $SED -e '/GOCART_INTERNAL_RESTART_FILE:/ s/ gocart_internal_rst/-gocart_internal_rst/' AGCM.rc
   fi
fi

if [[ $CHOU == TRUE ]]
then
   convert_rrtmg_none 
fi

if [[ $RRTMG_LW == TRUE ]]
then
   convert_rrtmg_lw 
fi

if [[ $RRTMG_SW == TRUE ]]
then
   convert_rrtmg_sw 
fi

if [[ $RRTMG == TRUE ]]
then
   convert_rrtmg_swlw 
fi

if [[ $ONEMO == TRUE ]]
then
   echo "Enabling single-moment in AGCM.rc"
   echo "CLDMICRO: 1MOMENT" >> AGCM.rc
fi

if [[ $TWOMO == TRUE ]]
then
   echo "Enabling single-moment in AGCM.rc"
   echo "CLDMICRO: 2MOMENT" >> AGCM.rc
fi

if [[ $WW3 == TRUE ]]
then
   echo "Enabling WW3 in AGCM.rc"
   echo "USE_WW3: 1" >> AGCM.rc
   echo "WRITE_WW3_RESTART: 0" >> AGCM.rc
fi


if [[ $MIC == TRUE ]]
then
   echo "Enabling for MIC in AGCM.rc"
   echo "SOLAR_LOAD_BALANCE: 0" >> AGCM.rc
fi

if [[ $REPLAY == TRUE ]]
then
   echo "Turning on regular replay"
   $SED -e "/^#   REPLAY_MODE: Regular/ s/#/ /" \
        -e "/REPLAY_MODE: Regular/{n;s/#/ /}" AGCM.rc

   if [[ $SITE == NAS ]]
   then
      echo "Found NAS. Transforming REPLAY directory"
      $SED -e "/^ *REPLAY_FILE/ s#/discover/nobackup/projects/gmao/share/gmao_ops#${UTIL_DIR}#" AGCM.rc
   fi
fi

if [[ $TINY == TRUE ]]
then

   # Before we turned of GOCART.data. We now provide a
   # tiny version of it in Tiny-BCs

   #$SED -e "/AERO_PROVIDER:/ s/GOCART.data  /None  /" \
        #-e "/pchem_clim_years:/ s/228/2/" AGCM.rc

   $SED -e "/pchem_clim_years:/ s/228/2/" AGCM.rc

fi

if [[ $NOCHECK == TRUE ]]
then
   $SED -e '/^#RECORD_FINAL:/ s/#RECORD_FINAL:  >>>RECFINL<<</RECORD_FINAL: NO/' AGCM.rc
fi

if [[ $OPSB == TRUE || $OPSPORT == TRUE || $OPSBIO == TRUE ]]
then
   $SED -e '/^SOLAR_DT:/ s/1800/3600/' \
        -e '/^IRRAD_DT:/ s/1800/3600/' \
        -e '/^OGCM_RUN_DT:/ s/1800/3600/' AGCM.rc

   $SED -e '/^IRRAD_DT:/ a GOCART_DT: 3600' AGCM.rc

   $SED -e '/^ *NX:/ s/16/15/' \
        -e '/^ *NY:/ s/96/360/' AGCM.rc

   $SED -e '/^#RECORD_FINAL:/ s/#RECORD_FINAL:  >>>RECFINL<<</RECORD_FINAL: NO/' \
        -e '/^#RECORD_FREQUENCY:/ s/#RECORD_FREQUENCY: 000000       000000/RECORD_FREQUENCY: 000000/' \
        -e '/^#RECORD_REF_DATE:/ s/#RECORD_REF_DATE: >>>REFDATE<<< >>>FCSDATE<<</RECORD_REF_DATE: 20150415/' \
        -e '/^#RECORD_REF_TIME:/ s/#RECORD_REF_TIME: >>>REFTIME<<< >>>FCSTIME<<</RECORD_REF_TIME: 030000/' AGCM.rc
fi

if [[ $OPSPORT == TRUE ]]
then
   $SED -e '/^RECORD_REF_DATE:/ s/2015/2000/' AGCM.rc
fi

if [[ $SATSIM == TRUE ]]
then
   $SED -e '/^USE_SATSIM_ISCCP: / s/0/1/' AGCM.rc
fi

print_changes "AGCM.rc"

# ---------------------
# regress/gcm_regress.j
# ---------------------

#if [[ $REPLAY == TRUE ]]
#then
   #echo "Altering regression test for replay"
   #$SED -e "/test_duration = 21/ s/21/18/" \
        #-e "/test_duration = 03/ s/03/06/" regress/gcm_regress.j
#fi

print_changes "regress/gcm_regress.j"

# -----------
# cap_restart
# -----------

if [[ "$DOING_GOCART" == "GOCART" ]]
then
   USING_NR=$(grep DU_OPTICS AGCM.rc | grep NR)
   if [[ $? -eq 0 ]]
   then
      echo "     You seem to be using the Nature Run GOCART."
      echo "     Setting cap_restart to be in 2005"
      echo ""

      restore_save "cap_restart"
      copy_save "cap_restart"

      $SED -e "/^2000/ s/2000/2005/" cap_restart

      print_changes "cap_restart"
   fi

   USING_OPS=$(grep DU_OPTICS AGCM.rc | grep g5chem)
   if [[ $? -eq 0 ]]
   then
      echo "     You seem to be using the OPS GOCART."
      echo "     Setting cap_restart to be in 2015"
      echo ""

      restore_save "cap_restart"
      copy_save "cap_restart"

      $SED -e "/^2000/ s/2000/2015/" cap_restart

      print_changes "cap_restart"
   fi
fi

if [[ "$OCEAN_RES" == "Ostia-CS" ]]
then
   echo "     You seem to be using the Ostia-CS Ocean."
   echo "     Setting cap_restart to be in 2015"
   echo ""

   restore_save "cap_restart"
   copy_save "cap_restart"

   $SED -e "/^2000/ s/2000/2015/" cap_restart

   print_changes "cap_restart"
fi


# ------------------------
# RC/GEOS_ChemGridComp.rc
# ------------------------

if [[ $TINY == TRUE ]]
then

   # Before we turned of GOCART.data. We now provide a
   # tiny version of it in Tiny-BCs

   #echo "Turning off GOCART.data and TR in GEOS_ChemGridComp"
   #$SED -e "/ENABLE_GOCART_DATA:/ s/TRUE/FALSE/" \
        #-e "/ENABLE_TR:/ s/TRUE/FALSE/" RC/GEOS_ChemGridComp.rc

   echo "Turning off TR in GEOS_ChemGridComp"
   $SED -e "/ENABLE_TR:/ s/TRUE/FALSE/" RC/GEOS_ChemGridComp.rc
   
   print_changes "RC/GEOS_ChemGridComp.rc"
fi

if [[ $HEMCO == TRUE ]]
then

   echo "Turning on HEMCO in GEOS_ChemGridComp"
   $SED -e "/ENABLE_HEMCO:/ s/FALSE/TRUE/" RC/GEOS_ChemGridComp.rc
   
   print_changes "RC/GEOS_ChemGridComp.rc"
fi

if [[ $OPSB == TRUE || $OPSBIO == TRUE ]]
then

   echo "Turning on GAAS in GEOS_ChemGridComp"
   $SED -e "/ENABLE_GAAS:/ s/FALSE/TRUE/" RC/GEOS_ChemGridComp.rc
   
   print_changes "RC/GEOS_ChemGridComp.rc"
fi

# -------------------
# RC/GAAS_GridComp.rc
# -------------------

if [[ $OPSB == TRUE || $OPSBIO == TRUE ]]
then

   $SED -e "/^aod_ana_filename:/ s#aod_a.sfc.%y4%m2%d2_%h200z.nc4#$UTIL_DIR/GAAS_Input/d5124_m2_jan10.aod_a.sfc.%y4%m2%d2_%h200z.nc4#" \
        -e "/^aod_bkg_filename:/ s#aod_f.sfc.%y4%m2%d2_%h200z.nc4#$UTIL_DIR/GAAS_Input/d5124_m2_jan10.aod_f.sfc.%y4%m2%d2_%h200z.nc4#" \
        -e "/^aod_avk_filename:/ s#aod_k.sfc.%y4%m2%d2_%h200z.nc4#$UTIL_DIR/GAAS_Input/d5124_m2_jan10.aod_k.sfc.%y4%m2%d2_%h200z.nc4#" RC/GAAS_GridComp.rc
   
   print_changes "RC/GAAS_GridComp.rc"
fi

# -------------
# src directory
# -------------

if [[ -d src ]]
then
   echo "src directory found. tarring to save inodes"
   tar cf src.tar src
   if [[ -x $PBZIP2 ]]
   then
      echo "pbzip2 found: $PBZIP2, compressing"
      $PBZIP2 -l src.tar
   fi
   echo "removing src directory"
   rm -rf src
fi
