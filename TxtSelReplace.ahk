TxtSelReplace(action="") {
	global RX_SCITXT:=""
	hwnd:= winexist("A")
	winget,pid,pid,ahk_id %hwnd%
	ControlGetFocus,cname,ahk_id %hwnd%
	if(eRRORlEVEL)
		return,
	ControlGet,cWnd,Hwnd,,% cname,ahk_id %hwnd%
	if(instr(cname,"scintilla")) {
		sendmessage,2006,"","",,ahk_id %cWnd% ;GETLENGTH:=2006
		if(!Sci_FullLen:= errorlevel)
			return,
		VarSetCapacity(RX_SCITXT,Sci_FullLen,0) ; VarSetCapacity(selly,(Sci_FullLen:= errorlevel)+1,0)
		Sci_SelTxtPTR:= DllCall("VirtualAllocEx","Ptr",(hProc:= DllCall("OpenProcess","UInt",0x438,"Int",False,"UInt",PID
		,"Ptr")),"Ptr",0,"UPtr",Sci_FullLen,"UInt",0x1000,"UInt",4,"Ptr")
		if(action="Unquote") {
			sendmessage,2143,0,0,,ahk_id %cWnd%
			sendmessage,2190,% selstart:=errorlevel,0,,ahk_id %cWnd%	;SETTARGETSTART:=2190
			sendmessage,2145,0,0,,ahk_id %cWnd%
			sendmessage,2192,% selend:=errorlevel,0,,ahk_id %cWnd%		;SETTARGETEND:=2192
			if(selend-selstart>0)
				selstart--, selend++
			else,return
			sendmessage,2160,selstart-1,selend+1,,ahk_id %cWnd% ;SETSEL:=2160
		}
		if(action="Quote") {
			sendmessage,2143,0,0,,ahk_id %cWnd%
			sendmessage,2190,% selstart:=errorlevel,0,,ahk_id %cWnd% ;SETTARGETSTART:=2190
			sendmessage,2145,0,0,,ahk_id %cWnd%
			sendmessage,2192,% selend:=errorlevel,0,,ahk_id %cWnd%	 ;SETTARGETEND:=2192
			if(!(selend-selstart>0))
				return,
			sendmessage,2160,selstart-1,selend+1,,ahk_id %cWnd%	;SETSEL:=2160
		}
		sendmessage,2161,"",Sci_SelTxtPTR,,ahk_id %cWnd%		;GETSELTEXT:=2161;
		success:= DllCall("ReadProcessMemory","Ptr",hProc,"Ptr",Sci_SelTxtPTR,"Ptr",&RX_SCITXT,"UPtr",Sci_FullLen,"Ptr","")
		SCITXTDECoDED:= byte2string("RX_SCITXT")
		if(Sci_FullLen) {
			switch,action {
				case,"upper" : sel2:=uppercase(SCITXTDECoDED)
				case,"lower" : sel2:=lowercase(SCITXTDECoDED)
				case,"Capitalise" : sel2:=Capitalise(SCITXTDECoDED)
				case,"CapitaliseWithWords" : sel2:=CapitaliseWithWords(SCITXTDECoDED)
				case,"CommentLine" : if sel2:=commentline(SCITXTDECoDED)
					 selendappend:= strlen(sel2)-strlen(SCITXTDECoDED)
				case,"invert" : sel2:= invert_case(SCITXTDECoDED)
				case,"reverse" : sel2:= Capitalise(SCITXTDECoDED)
				case,"Enclose_Brackets" : if(sel2:=Enclose_Brackets(SCITXTDECoDED))
					selendappend:=1, selstartappend:=1
				case,"Enclose_Square_Brackets" : if(sel2:=Enclose_Square_Brackets(SCITXTDECoDED))
					selendappend:=1, selstartappend:=1
				case,"Enclose_Braces" : if(sel2:=Enclose_Braces(SCITXTDECoDED))
					selendappend:=1, selstartappend:=1
				case,"Enclose_Percents" : if(sel2:=Enclose_Percents(SCITXTDECoDED))
					selendappend:=1, selstartappend:=1
				case,"Quote" : if(e:=instr(SCITXTDECoDED,chr(34))){
						if(sel2:=unEnquote(SCITXTDECoDED))
							action:="unquote", selendappend:= -2
					} else {
						sendmessage,2142,selstart,0,,ahk_id %cWnd%	;SETSELECTIONSTART:=2142;
						sendmessage,2144,selend,0,,ahk_id %cWnd% 	;SETSELECTIONEND:=2144;
						StringTrimleft,SCITXTDECoDED,SCITXTDECoDED, 1
						StringTrimright,SCITXTDECoDED,SCITXTDECoDED, 1
						if(sel2:=Enquote(SCITXTDECoDED))
							 selendappend:= 2
					}
				case,"UnQuote" : f:=instr(SCITXTDECoDED,chr(34))
					if(sel2:=unEnquote(SCITXTDECoDED))
						selendappend:= -2
			}
			sendmessage,2143,0,0,,ahk_id %cWnd% 
			sendmessage,2190,% selstart:=errorlevel,0,,ahk_id %cWnd% ;SETTARGETSTART:=2190
			sendmessage,2145,0,0,,ahk_id %cWnd% 
			sendmessage,2192,% selend:=errorlevel,0,,ahk_id %cWnd%  ;SETTARGETEND:=2192
			sendmessage,2160,selstart,selend,,ahk_id %cWnd%			;SETSEL:=2160
			if(!(len2:=selend-selstart)>0)
				return,
			len2++	;considering null-term;
			VarSetCapacity(vAlloxStr,(len2)*2+1,0)
			, StrPut(sel2,&vAlloxStr,"UTF-8")
			, vAlloxAddress:= DllCall("VirtualAllocEx","Ptr",(hProc:= DllCall("OpenProcess","UInt",0x438,"Int",False,"UInt",PID
			,"Ptr")),"Ptr",0,"UPtr",len2+1,"UInt",0x1000,"UInt",4,"Ptr")
			, success:= DllCall("WriteProcessMemory","Ptr",hProc,"Ptr",vAlloxAddress,"Ptr",&vAlloxStr,"Uint",len2+1,"UInt*","","Int")
			selendappend? selend+=selendappend:()
			selstartappend? selstart+=selstartappend:()
			sendmessage,2170,0,vAlloxAddress,,ahk_id %cWnd%	;REPLACESEL:=2170;
			(action="unquote")? ((e!=1||f)? (selstart++, selend--):()):() ;else,if(action="Enclose_Brackets") ;selstart++,selend-=1
			sendmessage,2142,selstart,0,,ahk_id %cWnd%		;(SETSELECTIONSTART):=2142;
			sendmessage,2144,selend,0,,ahk_id %cWnd%		;SETSELECTIONEND:=2144;  
			DllCall("VirtualFreeEx","Ptr",hProc,"Ptr",vAlloxAddress,"UPtr",0,"UInt",0x8000) ;MEM_RELEASE
			, DllCall("VirtualFreeEx","Ptr",hProc,"Ptr",Sci_SelTxtPTR,"UPtr",0,"UInt",0x8000)
			,  DllCall("CloseHandle","Ptr",hProc)
		}
	} else,if(instr(cname,"edit")) { ;(!Edit_TextIsSelected(cWnd)? return());
		ControlGet,sel,Selected,,,ahk_id %cWnd%
		if(!strlen(sel))
			return,
		switch,action {
			case,"upper"		: sendmessage,0xC2,1,&sel:= Uppercase(sel)	,,ahk_id %cWnd% ;EM_REPLACESEL;0xc2;
			case,"lower"		: sendmessage,0xC2,1,&sel:= lowercase(sel)	,,ahk_id %cWnd%
			case,"invert" 		: sendmessage,0xC2,1,&sel:= invert_case(sel),,ahk_id %cWnd%
			case,"reverse" 		: sendmessage,0xC2,1,&sel:= Capitalise(sel)	,,ahk_id %cWnd%
			case,"commentline"	: sendmessage,0xC2,1,&sel:= Capitalise(sel)	,,ahk_id %cWnd%
			case,"Capitalise"	: sendmessage,0xC2,1,&sel:= Capitalise(sel)	,,ahk_id %cWnd%
			case,"CapitaliseWithWords" : sendmessage,0xC2,1,&sel:= Capitalise(sel),,ahk_id %cWnd%
		}
	}
	return,1
}

UPPERCASE(target="") { 
 return,regexreplace(target,"(\w)","$U1$2")
}

lowercase(target="") {
 return,regexreplace(target,"(\w)","$L1")
}

iNVERT_cASE(Target="") { ;toggle;
 return,regexreplace(target,"([A-Z])|([a-z])","$L1$U2")
}

CommentLine(target="") { ;toggle;
 return,regexreplace(regexreplace(target,"(\n?)(.+)","$1;$2"),"(;[\s]*[;]+)","")
}

Enclose_Brackets(target="") {
 return,regexreplace(target,"(.*[\n\r]*)","($1)")
}

Enclose_Square_Brackets(target="") {
 return,regexreplace(target,"(.*[\n\r]*)","[$1]")
}

Enclose_Braces(target="") {
 return,regexreplace(target,"(.*[\n\r]*)","{$1}")
}

Enclose_Percents(target="") {
 return,regexreplace(target,"(.*[\n\r]*)","%$1%")
}

Surround(input="",SurroundWith="") {
 (SurroundWith="quote"||SurroundWith="quotes"? SurroundWith:= chr(34))
 return,r:= (input=""? SurroundWith : SurroundWith . input . SurroundWith)
}

Enquote(target="") { ;toggle;
 return,regexreplace(target,"(.*[\n\r]*)", surround("$1","quotes")) ;chr(34) "$1" chr(34
}

UnEnquote(target="") { ; ="(?:^)(" chr(34) ")|(?:^.)(" chr(34) ")|(" chr(34) ")(?:$)|(" chr(34) ")(?:.$)"
 return,regexreplace(target,chr(34),"") ;this func not required
}

Capitalise(target="") { ;(\b\w)(.*?)||(s(?=cript))
 return,regexreplace(target,"(\b\w)(.*?)","$U1$2")
}

CapitaliseWithWords(target="") { ;regexreplace(target,"(((\b\w)(.*?))|(s)(?=cript))","$U1")
 static X:= "I,AHK,AutoHotkey,Var,Obj,replace,get," ;always Capitalised;
 S:= RegExReplace(RegExReplace(target,"(.*)","$L{1}"),"(?<=[\.\!\?]\s|\n).|^.","$U{0}")
 Loop,Parse,X, `, ;Parse the exceptions
  S:= RegExReplace(S,"i)\b" A_LoopField "\b", A_LoopField)
 return,S
}