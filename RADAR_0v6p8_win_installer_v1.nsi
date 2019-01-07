!include "MUI.nsh"
!include "WordFunc.nsh"		; WordReplace
!include "FileFunc.nsh"		; Locate
!include "UAC.nsh"		; 


; things that change version to version...
;-----------------------------------------
!define PACKAGE_DIR "RADAR_package_0v6p8"
Name "RADAR 0v6p8 Package"
OutFile "RADAR_0v6p8_win_installer.exe"


; defines for section selection / .onInit
;----------------------------------------
!define SF_USELECTED  0
;!define SF_SELECTED   1 .. already defined!
;!define SF_SECGRP     2
;!define SF_BOLD       8
;!define SF_RO         16
;!define SF_EXPAND     32

; macros for section selection / .onInit
;---------------------------------------
!macro SecSelect SecId
  Push $0
  IntOp $0 ${SF_SELECTED} | ${SF_RO}
  SectionSetFlags ${SecId} $0
  SectionSetInstTypes ${SecId} 1
  Pop $0
!macroend
 
!define SelectSection '!insertmacro SecSelect'
 
!macro SecUnSelect SecId
  Push $0
  IntOp $0 ${SF_USELECTED} | ${SF_RO}
  SectionSetFlags ${SecId} $0
  SectionSetText  ${SecId} ""
  Pop $0
!macroend
 
!define UnSelectSection '!insertmacro SecUnSelect'
 



; Attributes
;-----------
Var R_PATH		; R install path from Registry
Var RGUI_PATH		; Rgui.exe path from Locate
Var USE_OOCALC_CSV	; flag value in .Rprofile
Var CSV_R2CT		; flag value in .Rprofile
Var AS_ADMIN		; flag, set to 1 if doing R install

InstallDir $DOCUMENTS\RADAR
ShowInstDetails show

SetCompressor lzma

Var line1
Var line2

RequestExecutionLevel user
; one of user or admin
; -- if installing RADAR, need to be user
; -- if installing R, need to be admin
; -- need to set this prior to finding out if R needs to be installed!


; Installer Pages
; ----------------
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_LANGUAGE "English"




Section "Install R" SEC_1

	StrCpy $line1 "http://cran.at.r-project.org/bin/windows/base/R-2.15.0-win.exe"
	StrCpy $line2 "http://cran.at.r-project.org/bin/windows/base/old/2.15.0/R-2.15.0-win.exe"
	inetc::get "$line1" "$INSTDIR\R-2.15.0-win.exe"
	Pop $0
	StrCmp $0 "OK" done_R_download
	
	inetc::get  "$line2" "$INSTDIR\R-2.15.0-win.exe"
	Pop $0
	StrCmp $0 "OK" done_R_download

	messageBox MB_OK "Oh dear!  Failed to download R, try to download and install it manually"
	return

done_R_download:

	; now install R...
	; run the .exe file
	ExecWait "$INSTDIR\R-2.15.0-win.exe"

	readRegStr $R_PATH HKLM "SOFTWARE\R-core\R" InstallPath
	IfErrors No_R R_installed
No_R:
	messageBox MB_OK "Oh dear!  Failed to properly install R, try to download and install it manually"
	StrCpy $R_PATH "C:\Program Files\R\R-2.15.0"
	return

R_installed:

SectionEnd


Section "Install RADAR" SEC_2
	!insertmacro UAC_AsUser_Call Function Install_RADAR ${UAC_SYNCREGISTERS}|${UAC_SYNCINSTDIR}
SectionEnd


; ===================================================================
; Function Definitions
; ===================================================================
Function .onInit
	StrCpy $AS_ADMIN "0"
	messageBox MB_OK "Welcome to the RADAR package installer..."

	; check if R is installed already
	readRegStr $R_PATH HKLM "SOFTWARE\R-core\R" InstallPath
	IfErrors need_R have_R	
need_R:
	; can't get the elevation to Admin and then de-elevation back to user to
	; work properly, so for now, just say you need to install R, rather
	; than trying to find and install R
	
	;${SelectSection} ${SEC_1}
	;messageBox MB_OK "RADAR requires R to be installed $\r R will be downloaded and installed..."

	;!insertmacro UAC_RunElevated
	;StrCmp 1223 $0 UAC_ElevationAborted ; UAC dialog aborted by user?
	;StrCmp 0 $0 0 UAC_Err ; Error?
	;StrCmp 1 $1 0 UAC_Success ;Are we the real deal or just the wrapper?
	
	; new, brain-dead code...
	${UnSelectSection} ${SEC_1}
	messageBox MB_OK "RADAR requires R to be installed, get and install R, then rerun RADAR installer."
	Quit
;UAC_Err:
;	MessageBox mb_iconstop "Could not elevate process (errorcode $0), continuing with normal user privileges." /SD IDOK
;	goto done_check_R
;UAC_Success:
;	StrCpy $AS_ADMIN "1"
;UAC_ElevationAborted:
;	goto done_check_R
have_R:	
	;messageBox MB_OK "R is already installed..."
	${UnSelectSection} ${SEC_1}

;done_check_R:
	StrCpy $line1 "Now a couple of default setting questions...$\r"
	StrCpy $line2 "(You can always change these later)"
	messageBox MB_OK "$line1$line2"
	StrCpy $USE_OOCALC_CSV "1"
	StrCpy $line1 "Default CSV formula format is for LibreOfficeCalc. (OpenOfficeCalc)$\r"
	StrCpy $line2 "Change default format to Microsoft Excel instead?"
	messageBox MB_YESNO "$line1$line2" IDNO using_calc
	StrCpy $USE_OOCALC_CSV "0"
using_calc:
	StrCpy $CSV_R2CT '"r2c"'
	StrCpy $line1 "Default ConvertCsv output is rows=tests, cols=parts $\r"
	StrCpy $line2 "Change this to rows=parts, cols=tests?"
	messageBox MB_YESNO "$line1$line2" IDNO no_transpose
	StrCpy $CSV_R2CT '"r2ct"'
no_transpose:

FunctionEnd

;===============================================

Function Set_RGUI_PATH

	;MessageBox MB_OK "found... $R9"
	StrCpy $RGUI_PATH $R9

FunctionEnd

;===============================================
; was in Section "Install RADAR",
; but moved into Function so can use UAC as user call
Function Install_RADAR

	; move RADAR package files to install folder
	;--------------------------------------------
	setOutPath $INSTDIR
	File /r "${PACKAGE_DIR}"	

	; update .Rprofile file
	;-----------------------
	; check if .Rprofile already exists, save older versions
	IfFileExists $INSTDIR\old5.Rprofile 0 +1
	  Delete $INSTDIR\old5.Rprofile
	IfFileExists $INSTDIR\old4.Rprofile 0 +1
	  Rename $INSTDIR\old4.Rprofile $INSTDIR\old5.Rprofile
	IfFileExists $INSTDIR\old3.Rprofile 0 +1
	  Rename $INSTDIR\old3.Rprofile $INSTDIR\old4.Rprofile
	IfFileExists $INSTDIR\old2.Rprofile 0 +1
	  Rename $INSTDIR\old2.Rprofile $INSTDIR\old3.Rprofile
	IfFileExists $INSTDIR\old.Rprofile 0 +1
	  Rename $INSTDIR\old.Rprofile $INSTDIR\old2.Rprofile
	IfFileExists $INSTDIR\.Rprofile 0 +1
	  Rename $INSTDIR\.Rprofile $INSTDIR\old.Rprofile
	
	; place new .Rprofile in install directory
	;Rename "${PACKAGE_DIR}\nsis.Rprofile" $INSTDIR\.Rprofile
	File /oname=$INSTDIR\.Rprofile "${PACKAGE_DIR}\nsis.Rprofile"

	; update .Rprofile with correct installed location
	Var /GLOBAL radar_path 
	StrCpy $radar_path "$INSTDIR\${PACKAGE_DIR}"
	; convert \'s to /'s
	${WordReplace} $radar_path "\" "/" "+" $radar_path

	Push @INSTALLED_PKG_DIR@
	Push $radar_path
	Push all
	Push all
	Push $INSTDIR\.Rprofile
	Call AdvReplaceInFile
	
	; update .Rprofile with correct OOCalc vs. MS Excel flag setting
	Push @USE_OOCALC_CSV@
	Push $USE_OOCALC_CSV
	Push all
	Push all
	Push $INSTDIR\.Rprofile
	Call AdvReplaceInFile

	; update .Rprofile with correct default_convcsv_type value
	Push @CSV_R2CT@
	Push $CSV_R2CT
	Push all
	Push all
	Push $INSTDIR\.Rprofile
	Call AdvReplaceInFile


	; create desktop shortcut
	; ------------------------
	; find Rgui.exe for creating Desktop shortcut...
	${Locate} "$R_PATH" "/L=F /M=Rgui.exe" "Set_RGUI_PATH"
	IfErrors +1 +2
	StrCpy $RGUI_PATH ""

	createShortCut "$DESKTOP\RADAR.lnk" \
	"$RGUI_PATH" "--sdi --no-save" 

FunctionEnd

;===============================================

Function AdvReplaceInFile
 
         ; call stack frame:
         ;   0 (Top Of Stack) file to replace in
         ;   1 number to replace after (all is valid)
         ;   2 replace and onwards (all is valid)
         ;   3 replace with
         ;   4 to replace
 
         ; save work registers and retrieve function parameters
         Exch $0 ;file to replace in
         Exch 4
         Exch $4 ;to replace
         Exch
         Exch $1 ;number to replace after
         Exch 3
         Exch $3 ;replace with
         Exch 2
         Exch $2 ;replace and onwards
         Exch 2
         Exch 
         Push $5 ;minus count
         Push $6 ;universal
         Push $7 ;end string
         Push $8 ;left string
         Push $9 ;right string
         Push $R0 ;file1
         Push $R1 ;file2
         Push $R2 ;read
         Push $R3 ;universal
         Push $R4 ;count (onwards)
         Push $R5 ;count (after)
         Push $R6 ;temp file name
         GetTempFileName $R6
         FileOpen $R1 $0 r ;file to search in
         FileOpen $R0 $R6 w ;temp file
                  StrLen $R3 $4
                  StrCpy $R4 -1
                  StrCpy $R5 -1
        loop_read:
         ClearErrors
         FileRead $R1 $R2 ;read line
         IfErrors exit
         StrCpy $5 0
         StrCpy $7 $R2
 
        loop_filter:
         IntOp $5 $5 - 1
         StrCpy $6 $7 $R3 $5 ;search
         StrCmp $6 "" file_write2
         StrCmp $6 $4 0 loop_filter
 
         StrCpy $8 $7 $5 ;left part
         IntOp $6 $5 + $R3
         StrCpy $9 $7 "" $6 ;right part
         StrCpy $7 $8$3$9 ;re-join
 
         IntOp $R4 $R4 + 1
         StrCmp $2 all file_write1
         StrCmp $R4 $2 0 file_write2
         IntOp $R4 $R4 - 1
 
         IntOp $R5 $R5 + 1
         StrCmp $1 all file_write1
         StrCmp $R5 $1 0 file_write1
         IntOp $R5 $R5 - 1
         Goto file_write2
 
        file_write1:
         FileWrite $R0 $7 ;write modified line
         Goto loop_read
 
        file_write2:
         FileWrite $R0 $R2 ;write unmodified line
         Goto loop_read
 
        exit:
         FileClose $R0
         FileClose $R1
 
         SetDetailsPrint none
         Delete $0
         Rename $R6 $0
         Delete $R6
         SetDetailsPrint both
 
         Pop $R6
         Pop $R5
         Pop $R4
         Pop $R3
         Pop $R2
         Pop $R1
         Pop $R0
         Pop $9
         Pop $8
         Pop $7
         Pop $6
         Pop $5
         Pop $4
         Pop $3
         Pop $2
         Pop $1
         Pop $0
FunctionEnd


