C-----------------------------------------------------------------------
      SUBROUTINE SPTRANF1(IROMB,MAXWV,IDRT,IMAX,JMAX,JB,JE,
     &                    EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP,
     &                    AFFT,CLAT,SLAT,WLAT,PLN,PLNTOP,MP,
     &                    W,WTOP,G,IDIR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRANF1   SPTRANF SPECTRAL TRANSFORM
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS AN SINGLE LATITUDE TRANSFORM FOR
C           SUBPROGRAM SPTRANF.  USE THIS SUBPROGRAM OUTSIDE
C           THE SPTRANF FAMILY CONTEXT AT YOUR OWN RISK.
C
C PROGRAM HISTORY LOG:
C 1998-12-15  IREDELL
C
C USAGE:    CALL SPTRANF1(IROMB,MAXWV,IDRT,IMAX,JMAX,JB,JE,
C    &                    EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP,
C    &                    AFFT,CLAT,SLAT,WLAT,PLN,PLNTOP,MP,
C    &                    W,WTOP,G,IDIR)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRT     - INTEGER GRID IDENTIFIER
C                (IDRT=4 FOR GAUSSIAN GRID,
C                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAX     - INTEGER EVEN NUMBER OF LONGITUDES
C     JMAX     - INTEGER NUMBER OF LATITUDES
C     JB       - INTEGER LATITUDE INDEX (FROM POLE) TO BEGIN TRANSFORM
C     JE       - INTEGER LATITUDE INDEX (FROM POLE) TO END TRANSFORM
C     EPS      - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C     EPSTOP   - REAL (MAXWV+1)
C     ENN1     - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C     ELONN1   - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C     EON      - REAL ((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
C     EONTOP   - REAL (MAXWV+1)
C     CLAT     - REAL (JB:JE) COSINES OF LATITUDE
C     SLAT     - REAL (JB:JE) SINES OF LATITUDE
C     WLAT     - REAL (JB:JE) GAUSSIAN WEIGHTS
C     AFFT     - REAL(8) (50000+4*IMAX) AUXILIARY ARRAY IF IDIR=0
C     PLN      - REAL ((M+1)*((I+1)*M+2)/2,JB:JE) LEGENDRE POLYNOMIALS
C     PLNTOP   - REAL (M+1,JB:JE) LEGENDRE POLYNOMIAL OVER TOP
C     MP       - INTEGER IDENTIFIER (0 FOR SCALAR, 1 FOR VECTOR)
C     W        - REAL (*) WAVE FIELD IF IDIR>0
C     WTOP     - REAL (*) WAVE FIELD OVER TOP IF IDIR>0
C     G        - REAL (IMAX,2,JB:JE) GRID FIELD IF IDIR<0
C     IDIR     - INTEGER TRANSFORM FLAG
C                (IDIR>0 FOR WAVE TO GRID, IDIR<0 FOR GRID TO WAVE)
C   OUTPUT ARGUMENTS:
C     W        - REAL (*) WAVE FIELD IF IDIR<0
C     WTOP     - REAL (*) WAVE FIELD OVER TOP IF IDIR<0
C     G        - REAL (IMAX,2,JB:JE) GRID FIELD IF IDIR>0
C
C SUBPROGRAMS CALLED:
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C   SPANALY      ANALYZE SPECTRAL FROM FOURIER
C   SPFFTE       PERFORM FAST FOURIER TRANSFORM
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      REAL(8) AFFT(*) ! Expecting (256+IMAX) in SPFFTESGI()
      REAL CLAT(JB:JE),SLAT(JB:JE),WLAT(JB:JE)
      REAL PLN((MAXWV+1)*((IROMB+1)*MAXWV+2)/2,JB:JE)
      REAL PLNTOP(MAXWV+1,JB:JE)
      REAL W((MAXWV+1)*((IROMB+1)*MAXWV+2))
      REAL WTOP(2*(MAXWV+1))
      REAL G(IMAX,2,JB:JE)
      REAL F(IMAX+2,2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!      write(0,*) 'sptranf1 top'
      KW=(MAXWV+1)*((IROMB+1)*MAXWV+2)
      KWTOP=2*(MAXWV+1)
      IF(IDIR.GT.0) THEN
        DO J=JB,JE
          CALL SPSYNTH(IROMB,MAXWV,IMAX,IMAX+2,KW,KWTOP,1,
     &                 CLAT(J),PLN(1,J),PLNTOP(1,J),MP,
     &                 W,WTOP,F)
          CALL SPFFTE(IMAX,(IMAX+2)/2,IMAX,2,F,G(1,1,J),+1,AFFT)
        ENDDO
      ELSE
        DO J=JB,JE
          CALL SPFFTE(IMAX,(IMAX+2)/2,IMAX,2,F,G(1,1,J),-1,AFFT)
          CALL SPANALY(IROMB,MAXWV,IMAX,IMAX+2,KW,KWTOP,1,
     &                 WLAT(J),CLAT(J),PLN(1,J),PLNTOP(1,J),MP,
     &                 F,W,WTOP)
        ENDDO
!      write(0,*) 'sptranf1 end'
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
