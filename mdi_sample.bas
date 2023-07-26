
' ---------------------------------------------------------------------------------------------------
/' 
 Title: mdi_sample.bas - Creates a IUP Dialog With a menu, toolbar And show mdi With tree And matrix
 Example in Freebasic
 Version: 1.0 - Julho 2023
 Author: Porthos Motta - portohs.motta@outlook.com
 License: GPL v3
 About: Gui version
 '/
' ---------------------------------------------------------------------------------------------------

#include once "IUP/iup.bi"
#Include once "IUP/iupcontrols.bi"
#Include once "IUP/iupkey.bi"
#Include once "IUP/iupcb.bi"
#include once "IUP/iupmatrix.bi"

' *****************************
'   Const Declarations
' *****************************

Const NULL = 0
Const NEWLINE = !"\n"

' *****************************
'   SUB and FUNCTION declarations
' *****************************

Declare Function mdi_new cdecl (ByVal handler As Ihandle Ptr) As Long 
Declare Sub createMenu()
Declare Function createFrame cdecl () As Ihandle Ptr
Declare Function mdi_activate cdecl (ByVal handler As Ihandle Ptr) As Integer
Declare Function button_cb cdecl (ByVal handler As Ihandle Ptr) As Integer

Declare Function  create_statusbar cdecl () As Ihandle Ptr
Declare Function selectnode cdecl (ByVal h As Ihandle Ptr) As Integer

' *****************************
'   VAR declaretions
' *****************************


Dim As Ihandle Ptr mat

' *****************************
'   SUB and FUNCTION code
' *****************************
 
Function selectnode(ByVal h As Ihandle Ptr) As Integer
	Dim As Ihandle Ptr tree, mat, dlg
	Dim i As Integer
	Dim s As String
	Dim id As Integer

	tree = IupGetHandle("tree")
	'IupSetAttribute(tree, "VALUE", IupGetAttribute(h, "TITLE"))
	mat = IupGetHandle("mat")
	'dlg = IupGetHandle("dlg")
    'id = IupGetInt(tree, "VALUE")
    'IupSetAttribute(mat, "CLEARVALUE", "ALL")	        
    IupSetAttribute(mat, "REDRAW", "ALL")    	

    If (IupGetInt(tree, "VALUE") = 0) Then
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "0")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")    	
    ElseIf (IupGetInt(tree, "VALUE") = 1) Then
	    'IupSetAttribute(mat, "CLEARATTRIB", "ALL")    
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "1")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
    ElseIf (IupGetInt(tree, "VALUE") = 2) Then
		IupSetAttribute(mat, "NUMLIN", "2") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "3")
		IupSetAttribute(mat, "1:0", "2")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
		'IupRefresh(dlg)  
	    IupSetAttribute(mat, "REDRAW", "ALL")    	
    ElseIf (IupGetInt(tree, "VALUE") = 3) Then				
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "3")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
	ElseIf (IupGetInt(tree, "VALUE") = 4) Then
		'IupSetAttribute(mat, "CLEARATTRIB", "ALL")    
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "4")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
    ElseIf (IupGetInt(tree, "VALUE") = 5) Then
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "5")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
    ElseIf (IupGetInt(tree, "VALUE") = 6) Then
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "6")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
    ElseIf (IupGetInt(tree, "VALUE") = 7) Then
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "7")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
    ElseIf (IupGetInt(tree, "VALUE") = 8) Then
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "8")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
    ElseIf (IupGetInt(tree, "VALUE") = 9) Then
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "9")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
    ElseIf (IupGetInt(tree, "VALUE") = 10) Then
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "10")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
    ElseIf (IupGetInt(tree, "VALUE") = 11) Then
		IupSetAttribute(mat, "NUMLIN", "1") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
		IupSetAttribute(mat, "1:0", "11")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")
    ElseIf (IupGetInt(tree, "VALUE") = 12) Then
		IupSetAttribute(mat, "NUMLIN", "3") 
		IupSetAttribute(mat, "NUMLIN_VISIBLE", "3")
		IupSetAttribute(mat, "1:0", "12")
		IupSetAttribute(mat, "1:1", "ROW1")
		IupSetAttribute(mat, "1:2", "ROW2")
		IupSetAttribute(mat, "1:3", "ROW3")
		IupSetAttribute(mat, "1:4", "ROW4")	
		IupSetAttribute(mat, "1:5", "ROW5")
		IupSetAttribute(mat, "1:6", "ROW6")
		IupSetAttribute(mat, "1:7", "ROW7")

    EndIf
	
    'IupMessage( "select", Str(IupTreeGetId(tree, 1))) 
    'IupMessage( "select", IupGetAttribute(tree, "TITLE")) 
    'Print IupGetAttribute(IupGetAttribute(tree, "TITLE"), "NAME")
	Print IupGetInt(tree, "VALUE")
	'IupMessage( "select", Str(IupGetName(mat)))   
	'IupMessage( "select", IupGetAttribute(tree,"VALUE")) 
	'Print IupGetAttribute(tree, "TITLE")
	'IupRefresh(dlg)  
    IupSetAttribute(mat, "REDRAW", "ALL")    	
	'IupRefresh(dlg)  
	'IupSetAttribute(mat, "CLEARVALUE", "ALL")
    'IupSetAttribute(mat, "REDRAW", "ALL")
	'IupRefresh(dlg)				    
	
 	Function = IUP_DEFAULT

 End Function
 

Function mdi_activate cdecl (ByVal handler As Ihandle Ptr) As Integer
	IupMessage("Created Menu", "Press OK" )   
	
	
	Function = IUP_DEFAULT
End Function

Function button_cb(ByVal handler As Ihandle Ptr) As Integer
	IupMessage( "CreatedMenu", "Press OK" )   
	IupMessageDlg()
	Function = IUP_DEFAULT

End Function


Function mdi_new cdecl (ByVal handler As Ihandle Ptr) As Long
	Dim id As Integer 
	id = 0
	Dim As Ihandle Ptr box, txt, txt1, txt2, dlg, bt, _cbox, mat
	Dim As Ihandle Ptr label1, label2, label3, label4, label5, label6, label7
	Dim As Ihandle Ptr  tree = IupTree()

    'IupSetAttribute(tree, "TIP", "Tree Tip")
	'IupSetAttribute(tree, "TITLE", "Figures")
	IupSetHandle("tree", tree)

	'IupSetHandle("tree", tree)
    
	mat = IupMatrix(NULL) 

    IupSetAttribute(mat, "EXPAND", "NO")
	IupSetAttribute(mat, "SCROLLBAR", "NO")
	IupSetAttribute(mat, "FLATSCROLLBAR", "NO")
	'IupSetAttribute(mat, "AUTOHIDE", "YES")
  	IupSetAttribute(mat, "SIZE", "HALFXHALF")
    IupSetAttribute(mat, "RESIZEMATRIX", "NO")
  	IupSetAttribute(mat, "SHRINK", "YES")
    IupSetAttribute(mat, "NUMCOL", "7")
	IupSetAttribute(mat, "NUMLIN", "1") 
	IupSetAttribute(mat, "NUMCOL_VISIBLE", "7")
	IupSetAttribute(mat, "NUMLIN_VISIBLE", "1")
	IupSetAttribute(mat, "BGCOLOR", "255 255 255")
	IupSetAttribute(mat, "BGCOLOR1:0", "247 249 254")
	IupSetAttribute(mat, "BGCOLOR1:1", "247 249 254")	
	IupSetAttribute(mat, "BGCOLOR1:2", "247 249 254")	
	IupSetAttribute(mat, "BGCOLOR1:3", "247 249 254")
	IupSetAttribute(mat, "BGCOLOR1:4", "247 249 254")					
	IupSetAttribute(mat, "BGCOLOR1:5", "247 249 254")					
	IupSetAttribute(mat, "BGCOLOR1:6", "247 249 254")							
	IupSetAttribute(mat, "BGCOLOR1:7", "247 249 254")								
	'IupSetAttribute(mat, "BGCOLORL:*", "255 128 0")
	'IupSetAttribute(mat, "BGCOLOR2:1", "255 128 0")
	'IupSetAttribute(mat, "FGCOLOR2:0", "255 0 128")
	'IupSetAttribute(mat, "FGCOLOR1:1", "255 0 128")
    IupSetAttribute(mat, "BGCOLOR" + "0" + ":" + "0", "231 238 254")
    IupSetAttribute(mat, "BGCOLOR" + "0" + ":" + "1", "233 238 254")
    IupSetAttribute(mat, "BGCOLOR" + "0" + ":" + "2", "233 238 254")    
    IupSetAttribute(mat, "BGCOLOR" + "0" + ":" + "3", "233 238 254")    
    IupSetAttribute(mat, "BGCOLOR" + "0" + ":" + "4", "233 238 254")    
    IupSetAttribute(mat, "BGCOLOR" + "0" + ":" + "5", "233 238 254")        
    IupSetAttribute(mat, "BGCOLOR" + "0" + ":" + "6", "233 238 254")    
    IupSetAttribute(mat, "BGCOLOR" + "0" + ":" + "7", "233 238 254")    
	'IupSetAttribute(mat, "CLEARVALUE", "ALL")	        
	'IupSetAttribute(mat, "CX", "1")

	IupSetAttribute(mat, "0:0", "#")
	IupSetAttribute(mat, "0:1", "COLUMN1")	
	IupSetAttribute(mat, "0:2", "COLUMN2")	
	IupSetAttribute(mat, "0:3", "COLUMN3")
	IupSetAttribute(mat, "0:4", "COLUMN4")
	IupSetAttribute(mat, "0:5", "COLUMN5")
	IupSetAttribute(mat, "0:6", "COLUMN6")
	IupSetAttribute(mat, "0:7", "COLUMN7")	
	
	'IupSetAttribute(mat, "CY", "250")
	IupSetHandle("mat", mat)
	
   _cbox = IupCbox(mat, NULL)
    IupSetAttribute(_cbox, "SIZE", "2000")
    IupSetAttribute(_cbox, "EXPAND", "YES")
    IupSetAttribute(_cbox, "MINISIZE", "1X10")
    
	txt1 = IupMultiLine( NULL)
	IupSetAttribute(txt1 , "VALUE", "Multiline Text\nSecond Line\nThird Line")
	IupSetAttribute(txt1 , "EXPAND", "YES")
	IupSetAttribute(txt1 , "SIZE", "80x40")
	IupSetAttribute(txt1 , "TIP", "Multiline TIP")

	txt2 = IupMultiLine( NULL)
	IupSetAttribute(txt2 , "VALUE", "Multiline Text\nSecond Line\nThird Line")
	IupSetAttribute(txt2 , "EXPAND", "YES")
	IupSetAttribute(txt2 , "SIZE", "80x40")
	IupSetAttribute(txt2 , "TIP", "Multiline TIP")

	bt = IupButton("new MDI", NULL)
	IupSetCallback(bt, "ACTION", Cast(Icallback, @button_cb))

	'box = IupVbox(_cbox,IupHbox(bt, NULL), txt1, txt2, NULL)
	box = IupHbox(tree, _cbox, NULL)
	IupSetAttribute(box, "MARGIN", "10x10")
	IupSetAttribute(box, "GAP", "1")
	IupSetAttribute(box, "EXPAND", "YES")
	IupSetAttribute(box, "EXPANDCHILDREN", "YES")

	dlg = IupDialog(box)
	IupSetfAttribute(dlg, "TITLE", "MDI Child (%d)", id)
	id = id + 1 
	IupSetAttribute(dlg, "MDICHILD", "YES")
	IupSetAttribute(dlg, "PARENTDIALOG", "mdiFrame")
	IupSetCallback(dlg, "MDIACTIVATE_CB", @mdi_activate)
	IupSetAttribute(dlg, "RASTERSIZE", "600x500")
    IupSetAttribute(dlg, "PLACEMENT", "MAXIMIZED")
	IupShow(dlg)

	'Tree sempre deve ser inicializada depois de chamar o dialogo
	
	IupSetAttributeId(tree, "TITLE", 0, "ROOT")
	IupSetAttributeId(tree, "ADDBRANCH", 0, "3")
	IupSetAttributeId(tree, "ADDLEAF", 1, "3.1")
	IupSetAttributeId(tree, "ADDLEAF", 2, "3.2")
	IupSetAttributeId(tree, "ADDBRANCH", 0, "2")
	IupSetAttributeId(tree, "ADDLEAF", 1, "2.1")
	IupSetAttributeId(tree, "ADDLEAF", 2, "2.2")
	IupSetAttributeId(tree, "ADDLEAF", 3, "2.3")
	IupSetAttributeId(tree, "ADDBRANCH", 0, "1")
	IupSetAttributeId(tree, "ADDLEAF", 1, "1.1")
	IupSetAttributeId(tree, "ADDLEAF", 2, "1.2")
	IupSetAttributeId(tree, "ADDLEAF", 3, "1.3")
	IupSetAttributeId(tree, "ADDLEAF", 4, "1.4")
	
	
	'IupSetFunction(tree, Cast(Icallback, @selectnode))
	IupSetCallback(tree, "SELECTION_CB", Cast(Icallback, @selectnode))		
	IupSetAttribute(dlg, "RASTERSIZE", NULL)
	IupMessage( "select", IupGetAttribute(tree, "TITLE")) 
	'IupMessage( "select", IupGetAttributeId(tree, "TITLE", 3)) 
	  
	'IupSetAttribute(mat, "REDRAW", "ALL")    	
	
	Function = IUP_DEFAULT

End Function

Sub createMenu()
	Dim As Ihandle Ptr winmenu
	Dim As Ihandle Ptr mnu, submenu, options, item_option
	
	item_option = IupItem("New", "mdi_new")
	IupSetAttribute(item_option, "KEY", "N")  
	
	options = IupMenu(item_option, NULL)
	submenu = IupSubmenu("MDI", options)
	mnu = IupMenu(submenu, NULL)
	IupSetHandle("mnu", mnu)

	IupMessage( "Welcome!", "Press OK" )   
	IupSetCallback (item_option, "ACTION", Cast(Icallback, @mdi_new))    ' attaches action to FUNCTION
 
End Sub

Function createFrame() As Ihandle Ptr
	Dim As Ihandle Ptr dlg, cnv, box, box2, bt, statusbar, label1, label2, fill
	
	cnv = IupCanvas( NULL)
	IupSetAttribute(cnv, "MDICLIENT", "YES")
	IupSetAttribute(cnv, "MDIMENU", "mdiMenu")
	'IupSetAttribute(cnv, "BGCOLOR", "128 255 0")
	
	bt = IupButton("Open MDI", NULL)
	IupSetCallback(bt, "ACTION", @button_cb)
	IupSetCallback (bt, "ACTION", @mdi_new)    ' attaches action to FUNCTION

	box = IupHbox(bt, NULL)
	IupSetAttribute(box, "MARGIN", "5x5")
	
	label1 = IupLabel(Time)

	fill = IupFill()
	IupSetAttribute(fill, "SIZE", "2x0")

	label2 = IupLabel(" | Welcome to Sample")

	box2 = IupHbox(label1, fill , label2, NULL)
	IupSetAttribute(box2 , "MARGIN", "5x5")
    IupSetAttribute(box2 , "GAP", "2")
	
	dlg = IupDialog(IupVbox(box, cnv, box2,  NULL))
	IupSetAttribute(dlg, "TITLE", "MDI Frame")
	IupSetAttribute(dlg, "MDIFRAME", "YES")
	IupSetAttribute(dlg, "RASTERSIZE", "800x600")
	IupSetAttribute(dlg, "MENU", "mnu")
	IupSetHandle("mdiFrame", dlg)
	
	Function = dlg
	
End Function

	Dim As Ihandle Ptr dlg
	
	IupOpen(NULL, NULL)
	
	'IupSetGlobal("UTF8MODE", "YES")
	'IupSetGlobal("DEFAULTFONT", "Segoe UI, 12")
	IupControlsOpen()
    
    'arvore = IupGetAttributeId(tree, "TITLE",0)
	IupSetAttribute(mat, "WIDTHDEF", "80")
	IupSetAttribute(mat, "WIDTHn", "80")
	IupSetAttribute(mat, "HEIGHDEF", "10")
	
	IupSetAttribute(mat, "CX", "5")
	IupSetAttribute(mat, "CY", "1")
	IupSetAttribute(mat, "READONLY", "YES")
		
	createMenu()
	dlg = createFrame()


	
	IupShowXY(dlg, IUP_CENTER, IUP_CENTER)  ' Displays the dialog
	IupSetAttribute(dlg, "RASTERSIZE", NULL)
	
	
	IupMainLoop()
	IupClose()
