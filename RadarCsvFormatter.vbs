Sub RadarCsvFormatter()
	'
	' $Id: RadarCsvFormatter.vbs,v 1.2 2023/12/20 01:53:37 david Exp $
	'
    ' Copyright (c) 2023 Davis Macadam, David Gattrell
	'
	'    This program is free software; you can redistribute it and/or modify
	'    it under the terms of the GNU General Public License as published by
	'    the Free Software Foundation; either version 2 of the License, or
	'    (at your option) any later version.
	'
	'    This program is distributed in the hope that it will be useful,
	'    but WITHOUT ANY WARRANTY; without even the implied warranty of
	'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	'    GNU General Public License for more details.
	'
	'    You should have received a copy of the GNU General Public License
	'    along with this program; if not, write to the Free Software
	'    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
	'
	'
	' David Gattrell   david.gattrell@gmail.com  25nov2023
	'
	' **********************************************************************
	' HOW TO RUN IN EXCEL:
	'
	' ---- Option 1: copy/paste code each time ----------------
	'
	' 1) open your RADAR generated .csv file in Excel
	'
	' 2) Alt + F11 -- Bring up the Visual Basic for Applications (VBA) editor
	'
	' 3) Right click on any item in tree view on left side of editor..
	'    Insert -> Module
	'    ... this will bring up a code window that you can paste *this* file into
	'
	' 4) Paste the contents of this file into the code window above
	'
	' 5) Alt + q -- you can now close/quit the VBA editor window
	'
	' 6) Alt + F8 -- will bring up the Macro selection window, select "RadarCsvFormatter" and click "Run."
	'
	' 7) When it has run (may take a second or up to 10s of seconds), you'll see a popup reminder to save as xlsx
	'    Save As ..  and save file as .xlsx
	'
	' ---- Option 2: create/use .xlsm file --------------------
	'
	' 0) First time only, creating .xlsm file
	'	A) Open empty Excel sheet
	'	B) steps 2,3,4,5 as above
	'	C) Save As ...  and select .xlsm extension   ... so you get RadarCsvFormatter.xlsm
	'
	' 1) If the xlsm file is not open in excel, do so 
	'
	' 2) open your RADAR generated .csv file in Excel
	'
	' 3) Alt + F8 -- bring up Macro selection window.. select RadarCsvFormatter.RadarCsvFormater and clikc "Run."
	' 
	' 4) step 7 as above  .. Save As .. and save file as .xlsx
	'
	'------------------------------------------------------------------------


    ' Search for "testnum" in Column B
    Dim testnumRow As Long
    testnumRow = Columns("B:B").Find("testnum").Row
    
    ' Search for "plot_ul" in the same row
    Dim plot_ulCol As Long
    plot_ulCol = Rows(testnumRow).Find("plot_ul").Column
    plot_llCol = plot_ulCol - 1


    ' Hide Column A, Row 1, and Column H
	'-----------------------------------
    Columns("A:A").Hidden = True
    Rows("1:1").Hidden = True
    Columns(plot_llCol).Hidden = True
    

    ' Adjust column widths
	'-----------------------
    Columns("C:C").ColumnWidth = 55
    Columns("D:E").ColumnWidth = 5
    Columns("F:G").ColumnWidth = 6
    

    ' Freeze Panes at the cell next to "plot_ul"
	'-------------------------------------------
    ' Unfreeze Panes
    ActiveWindow.FreezePanes = False

    Cells(testnumRow + 1, plot_ulCol + 1).Select
    ActiveWindow.FreezePanes = True
    

	' Grouping parameters from same 93K test_suites
	'----------------------------------------------
    ' Starting at the row beneath "testnum" in Column B
    Dim i As Long
    For i = testnumRow + 1 To Cells(Rows.Count, "B").End(xlUp).Row
        ' Search cells in Column C until ":" is found
        If InStr(1, Cells(i, 3).Value, ":") > 0 Then
            ' Compare contents with the next cells in Column C until a mismatch is found
            Dim prefix As String
            prefix = Left(Cells(i, 3).Value, InStr(1, Cells(i, 3).Value, ":") )
            
            Dim j As Long
            For j = i + 1 To Cells(Rows.Count, "C").End(xlUp).Row
                If Left(Cells(j, 3).Value, Len(prefix)) = prefix Then
                    ' Group rows until a cell is not a match
                    Rows(j).Rows.Group
                Else
                    Exit For
                End If
            Next j
            
            ' Skip to the next group
            i = j - 1
        End If
    Next i


	' Hardbin color formatting
	'--------------------------
    ' Search for "hard_bin" in Column I
    Dim hardBinRow As Long
    'hardBinRow = Columns("I:I").Find("hard_bin").Row
    hardBinRow = Columns(plot_ulCol).Find("hard_bin").Row
    
    ' Apply formatting to cells to the right of "hard_bin" in the same row
    If hardBinRow > 0 Then
        lastColumn = Cells(hardBinRow, Columns.Count).End(xlToLeft).Column
        Dim HbinRange As Range
        Set HbinRange = Range(Cells(hardBinRow, plot_ulCol + 1), Cells(hardBinRow, lastColumn))
        HbinRange.FormatConditions.Delete
        HbinRange.FormatConditions.Add Type:=xlCellValue, Operator:=xlNotBetween, _
                Formula1:="=0.5", Formula2:="=2.5"
        HbinRange.FormatConditions(1).Interior.Color = vbRed
        HbinRange.FormatConditions.Add Type:=xlCellValue, Operator:=xlNotBetween, _
                Formula1:="=0.5", Formula2:="=1.5"
        HbinRange.FormatConditions(2).Interior.Color = vbYellow
        HbinRange.FormatConditions.Add Type:=xlCellValue, Operator:=xlBetween, _
                Formula1:="=0.5", Formula2:="=1.5"
        HbinRange.FormatConditions(3).Interior.Color = RGB(0, 128, 0) ' dark Green, not vbGreen
    End If


	' Fail Result color formatting based on spreadsheet limits
	'---------------------------------------------------------
    ' Search for "ll" and "ul" in the same row
    Dim llCol As Long
    Dim ulCol As Long
    
    llCol = Rows(testnumRow).Find("ll").Column
    ulCol = Rows(testnumRow).Find("ul").Column
    
    ' Define the range of cells to process
    Dim dataRange As Range
    Set dataRange = Range(Cells(testnumRow + 1, plot_ulCol + 1), Cells(Cells(Rows.Count, ulCol).End(xlUp).Row, Columns.Count))
   	' when manually generated .. Use a formula ...
	' =AND(ISNUMBER(J15),ISNUMBER($F15),($F15>J15))
	' =AND(ISNUMBER(J15),ISNUMBER($G15),($G15<J15))
	'
	' & .... & is insert variable here in Formula ???
	dataRange.FormatConditions.Delete
	Application.ReferenceStyle = xlR1C1		'switch to Row/Column syntax
	dataRange.FormatConditions.Add Type:=xlExpression, _
			Formula1:="=AND(ISNUMBER(RC),ISNUMBER(RC" & llCol & "),(RC" & llCol & ">RC))"
	dataRange.FormatConditions(1).Interior.Color = vbRed
	dataRange.FormatConditions.Add Type:=xlExpression, _
			Formula1:="=AND(ISNUMBER(RC),ISNUMBER(RC" & ulCol & "),(RC" & ulCol & "<RC))"
	dataRange.FormatConditions(2).Interior.Color = vbRed

	Application.ReferenceStyle = xlA1		'switch back to default "A1" syntax



	' Add column with Test Indices for easier fail_test_indices searching
	'--------------------------------------------------------------------
	Range("B1").EntireColumn.Insert
	Cells("1","B") = 0.15
	Cells(testnumRow,"B") = "test_idx"
	lastRow = Cells(Rows.Count,"E").End(xlUp).Row	'testname column is now at E
	If lastRow > testnumRow + 2 Then
		Dim initRange As Range
		Set initRange = Range(Cells(testnumRow + 1,"B"), Cells(testnumRow + 2,"B"))
		Dim idxRange As Range
		Set idxRange = Range(Cells(testnumRow + 1,"B"), Cells(lastRow,"B"))
		Cells(testnumRow+1,"B") = 1
		Cells(testnumRow+2,"B") = 2
		initRange.AutoFill Destination:=idxRange, Type:=xlLinearTrend
	End If


	' remind user to save as XLSX
	'----------------------------
    ' Manually save the workbook as an Excel Workbook (.xlsx)
    MsgBox "Please save the workbook manually as an Excel Workbook (.xlsx)."
    
End Sub

