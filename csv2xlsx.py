#!/usr/bin/python
'''
$Id: csv2xlsx.py,v 1.2 2017/10/17 02:22:05 david Exp $

csv2xlsx.py

parse RADAR .csv file and generate .xlsx version with
conditional formatting for failing values and hbin not 1

usage:

	csv2xlsx.py my_file.csv					# will create 'my_file.xlsx'
	csv2xlsx.py my_file.csv new_name.xlsx	# will create 'new_name.xlsx'


NOTE: using tabs NOT 4 spaces

NOTE: xlsxwriter is not a 'base' python package.
			You will need to explicitly add it.

			Linux:
				sudo pip installl XlsxWriter
			MS:
				pip install XlsxWriter

			(And pip is not a 'base' python package in Ubuntu 16.04,
			 need to install python-pip)


# Copyright (C) 2017 David Gattrell
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
#  David.Gattrell
#  @
#  gmail.com

'''

import pdb				# python debugger

import sys				# for parsing input arguments 
import re				# for regular expressions
import csv				# for reading csv files
import xlsxwriter		# for writing xlsx files


debug1 = False
debug2 = False
debug3 = False

col_ch = [	'A','B','C','D','E','F','G','H',
			'I','J','K','L','M','N','O','P',
			'Q','R','S','T','U','V','W','X',
			'Y','Z','AA','AB','AC','AD','AE']



#-----------------------
# define some functions
#-----------------------
def is_integer(s):
	try:
		int(s)
		return True
	except ValueError:
		return False

def is_float(s):
	try:
		float(s)
		return True
	except ValueError:
		return False



#-----------------------------------
# process the command line arguments
#-----------------------------------
arg_idx = 0
csv_name = ''
xlsx_name = ''
for my_arg in sys.argv:
	arg_idx += 1
	if arg_idx<=1:
		continue		# name of script itself, skip this
	if my_arg[0]=='-':
		# -h or --help or whatever, just print usage info
		print 'Usage: csv2xlsx.py csv_filename [xlsx_filename]'
		sys.exit()
	if csv_name == '':	# this first arg should be csv_name
		csv_name = my_arg
		continue
	if xlsx_name == '':
		xlsx_name = my_arg
		break

if csv_name == '':
	csv_name = 'input.csv'

if xlsx_name == '':
	# build name based on csv_name
	xlsx_name = re.sub('(\.csv)?$','.xlsx',csv_name)


if debug2:
	print 'csv_name is ',csv_name
	print 'xlsx_name is ',xlsx_name



#---------------------
# read in the csv file
#---------------------
csvfile = open(csv_name,'rb')
csv_rows = csv.reader(csvfile, delimiter=',',quotechar='"')

my_rows = []
for row in csv_rows:
	my_rows.append(row)		

csvfile.close()



#----------------------------------
# dump csv file into xlsx worksheet
#----------------------------------
workbook = xlsxwriter.Workbook(xlsx_name)
worksheet = workbook.add_worksheet('from RADAR')

for row in range(len(my_rows)):
	for col in range(len(my_rows[row])):
		my_cell = my_rows[row][col]
		if is_integer(my_cell):
			worksheet.write(row,col,int(my_cell))
		elif is_float(my_cell):
			if float('-inf') < float(my_cell) < float('inf'):
				worksheet.write(row,col,float(my_cell))
			else:
				worksheet.write(row,col,'')
		else:
			worksheet.write(row,col,my_cell)



#------------------------------------------
# determine csv file orientation, 
# location of limits, hbins, results matrix
#------------------------------------------
row_count = len(my_rows)
col_count = len(my_rows[0])

transposed = False
if(my_rows[0][col_count-1] == '2'):
	transposed = True

if debug3:
	pdb.set_trace()

if not transposed:
	# confirm column/x...

	# ll/ul in cols 0.6 and 0.7...  (row 0.99 -> 'll' and 'ul'
	ll_x = my_rows[0].index('0.6')
	ul_x = my_rows[0].index('0.7')

	# plot_ll col 0.8  (row 0.99 -> 'plot_ll'
	plot_ll_x = my_rows[0].index('0.8')		# to hide this column
	scaler_x = my_rows[0].index('0.4')		# to make this column narrow
	units_x = my_rows[0].index('0.5')		# to make this column narrow

	# hbins in row 0.8  (col 0.9 -> 'hard_bin')
	hbin_x = my_rows[0].index('0.9')

	# testnames in col 0.3  (row 0.99 -> 'testname'
	tname_x = my_rows[0].index('0.3')

	# matrix first col 1, row 2  (J13 usually)
	mtx_x = my_rows[0].index('1')


	# confirm row/y...

	for row in range(row_count):
		if (my_rows[row][0] == '0.8'):
			hbin_y = row
		if (my_rows[row][0] == '0.99'):
			ll_y = ul_y = plot_ll_y = tname_y = row
		if (my_rows[row][0] == '2'):
			mtx_y = row
			break
else:	# transposed...
	hbin_x = my_rows[0].index('0.8')
	ll_x = ul_x = plot_ll_x = tname_x = my_rows[0].index('0.99')
	mtx_x = my_rows[0].index('2')

	for row in range(row_count):
		if (my_rows[row][0] == '0.3'):
			tname_y = row
		if (my_rows[row][0] == '0.6'):
			ll_y = row
		if (my_rows[row][0] == '0.7'):
			ul_y = row
		if (my_rows[row][0] == '0.9'):
			hbin_y = row
		if (my_rows[row][0] == '1'):
			mtx_y = row
			break


if debug1:
	print 'll_x is ',ll_x
	print 'ul_x is ',ul_x
	print 'tname_x is ',tname_x
	print 'mtx_x is ',mtx_x
	print 'hbin_y is ',hbin_y
	print 'll_y is ',ll_y
	print 'ul_y is ',ul_y
	print 'mtx_y is ',mtx_y


# sanity check these coordinates
ok_flag = True		# innocent until proven guilty

if (my_rows[ll_y][ll_x] != 'll'):
	ok_flag = False
	print 'confused looking for ll, found:',my_rows[ll_y][ll_x]

if (my_rows[ul_y][ul_x] != 'ul'):
	ok_flag = False
	print 'confused looking for ul'



#---------------------------------------------
# add formatting and conditional formatting
#---------------------------------------------
format_fail = workbook.add_format({'bg_color': 'red'})
format_pass = workbook.add_format({'bg_color': 'green'})

if not transposed:
	worksheet.set_column(0,0,2)					# make first col narrow 
	worksheet.set_column(scaler_x,scaler_x,5)	# make scaler narrow 
	worksheet.set_column(units_x,units_x,5)		# make units narrow 

	# make testname column wider, up to 63 char?, based on longest testname
	testname_width = 8
	for row in range(mtx_y,row_count):
		if len(my_rows[row][tname_x])>testname_width:
			testname_width = len(my_rows[row][tname_x])
			if testname_width>63:
				testname_width = 63
				break
	worksheet.set_column(2,2,testname_width)	# start col, stop col, width

	# hide ll_plot column
	worksheet.set_column(plot_ll_x,plot_ll_x,None,None,{'hidden': 1})

	# freeze panes
	worksheet.freeze_panes(mtx_y,mtx_x)

	# conditional format for hbin
	worksheet.conditional_format(hbin_y,mtx_x,hbin_y,col_count-1,{
			'type': 'cell',
			'criteria': 'not between',
			'minimum': 0.5,
			'maximum': 1.5,
			'format': format_fail})
	worksheet.conditional_format(hbin_y,mtx_x,hbin_y,col_count-1,{
			'type': 'cell',
			'criteria': 'between',
			'minimum': 0.5,
			'maximum': 1.5,
			'format': format_pass})

	# conditional format for results
	my_cell = col_ch[mtx_x] + str(mtx_y+1)		# 'J13'
	my_ll = '$' + col_ch[ll_x] + str(mtx_y+1)	# '$F13'
	my_ul = '$' + col_ch[ul_x] + str(mtx_y+1)	# '$G13'

	my_formula = '=AND(ISNUMBER(' + my_cell + '),ISNUMBER(' \
			+ my_ul + '),(' + my_ul + '<' + my_cell + '))'
	worksheet.conditional_format(mtx_y,mtx_x,row_count-1,col_count-1,{
			'type': 'formula',
			'criteria': my_formula,
			'format': format_fail})

	my_formula = '=AND(ISNUMBER(' + my_cell + '),ISNUMBER(' \
			+ my_ll + '),(' + my_ll + '>' + my_cell + '))'
	worksheet.conditional_format(mtx_y,mtx_x,row_count-1,col_count-1,{
			'type': 'formula',
			'criteria': my_formula,
			'format': format_fail})

else:		# transposed
	# freeze panes
	worksheet.freeze_panes(mtx_y,mtx_x)

	# conditional format for hbin
	worksheet.conditional_format(mtx_y,hbin_x,row_count-1,hbin_x,{
			'type': 'cell',
			'criteria': 'not between',
			'minimum': 0.5,
			'maximum': 1.5,
			'format': format_fail})
	worksheet.conditional_format(mtx_y,hbin_x,row_count-1,hbin_x,{
			'type': 'cell',
			'criteria': 'between',
			'minimum': 0.5,
			'maximum': 1.5,
			'format': format_pass})

	# conditional format for results
	my_cell = col_ch[mtx_x] + str(mtx_y+1)		# 'J13'
	my_ll = col_ch[mtx_x] + '$' + str(ll_y+1)	# '$F13'
	my_ul = col_ch[mtx_x] + '$' + str(ul_y+1)	# '$G13'

	my_formula = '=AND(ISNUMBER(' + my_cell + '),ISNUMBER(' \
			+ my_ul + '),(' + my_ul + '<' + my_cell + '))'
	worksheet.conditional_format(mtx_y,mtx_x,row_count-1,col_count-1,{
			'type': 'formula',
			'criteria': my_formula,
			'format': format_fail})

	my_formula = '=AND(ISNUMBER(' + my_cell + '),ISNUMBER(' \
			+ my_ll + '),(' + my_ll + '>' + my_cell + '))'
	worksheet.conditional_format(mtx_y,mtx_x,row_count-1,col_count-1,{
			'type': 'formula',
			'criteria': my_formula,
			'format': format_fail})



#---------
# tidy up
#---------
workbook.close()

