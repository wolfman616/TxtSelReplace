TxtSelReplace(action="") {
	global RX_SCITXT:=""
	hwnd:= winexist("A")
	winget,pid,pid,ahk_id %hwnd%
	ControlGetFocus,cname,ahk_id %hwnd%
	if(eRRORlEVEL)
		return,0
	ControlGet,cWnd,Hwnd,,% cname,ahk_id %hwnd%
	if(instr(cname,"scintilla")) {
		sendmessage,2006,"","",,ahk_id %cWnd% ;GETLENGTH:=2006;
		if(!Sci_FullLen:= errorlevel)
			return,0
		VarSetCapacity(RX_SCITXT,Sci_FullLen,0)
		Sci_SelTxtPTR:= DllCall("VirtualAllocEx","Ptr",(hProc:= DllCall("OpenProcess","UInt",0x438,"Int",False,"UInt",PID
		,"Ptr")),"Ptr",0,"UPtr",Sci_FullLen,"UInt",0x1000,"UInt",4,"Ptr")
		if(action="Unquote") {
			sendmessage,2143,0,0,,ahk_id %cWnd%
			sendmessage,2190,% SelStart:=errorlevel,0,,ahk_id %cWnd%	;SETTARGETSTART:=2190
			sendmessage,2145,0,0,,ahk_id %cWnd%
			sendmessage,2192,% SelEnd:=errorlevel,0,,ahk_id %cWnd%		;SETTARGETEND:=2192;
			if(SelEnd-SelStart>0)
				SelStart--, SelEnd++
			else,return,0
			sendmessage,2160,SelStart-1,SelEnd+1,,ahk_id %cWnd% ;SETSEL:=2160
		}
		if(action="Quote") {
			sendmessage,2143,0,0,,ahk_id %cWnd%
			sendmessage,2190,% SelStart:=errorlevel,0,,ahk_id %cWnd% ;SETTARGETSTART:=2190;
			sendmessage,2145,0,0,,ahk_id %cWnd%
			sendmessage,2192,% SelEnd:=errorlevel,0,,ahk_id %cWnd%	 ;SETTARGETEND:=2192;
			if(!(SelEnd-SelStart>0))
				return,0
			sendmessage,2160,SelStart-1,SelEnd+1,,ahk_id %cWnd%		;SETSEL:=2160;
		}
		sendmessage,2161,"",Sci_SelTxtPTR,,ahk_id %cWnd%			;GETSELTEXT:=2161;
		Success:= DllCall("ReadProcessMemory","Ptr",hProc,"Ptr",Sci_SelTxtPTR,"Ptr",&RX_SCITXT,"UPtr",Sci_FullLen,"Ptr","")
		SCiTxTrX:= Byte2Str("RX_SCITXT")
		if(!Sci_FullLen)
		 return,0
		 switch,action {
			case,		"upper"			: 	TX_t:= uppercase(SCiTxTrX)
			case,		"lower"			: 	TX_t:= lowercase(SCiTxTrX)
			case,		"Capitalise"	: 	TX_t:= Capitalise(SCiTxTrX)
			case,"CapitaliseWithWords"	:	TX_t:= CapitaliseWithWords(SCiTxTrX)
			case,		"CommentLine"	: ((TX_t:= commentline(SCiTxTrX))? 					(EndAppend:= Strlen(TX_t)-strlen(SCiTxTrX)))
			case,		"invert"		:	(isInt(SCiTxTrX)? (EndAppend:=strlen(TX_t:= FormatHex(SCiTxTrX))-strlen(SCiTxTrX)) : TX_t:= invert_case(SCiTxTrX))
			case,		"reverse"		:	TX_t:= Capitalise(SCiTxTrX)
			case,"Enclose_Brackets"		: ((TX_t:= Enclose_Brackets(SCiTxTrX))? 			(EndAppend:=1, StartAppend:= 1))
			case,"Enclose_Square_Brackets"	: ((TX_t:= Enclose_Square_Brackets(SCiTxTrX))?	(EndAppend:=1, StartAppend:= 1))
			case,"Enclose_Braces" 			: ((TX_t:= Enclose_Braces(SCiTxTrX))? 			(EndAppend:=1, StartAppend:= 1))
			case,"Enclose_Percents"			: ((TX_t:= Enclose_Percents(SCiTxTrX))? (		 EndAppend:=1, StartAppend:= 1))
			case,			"Quote"			: if(instr(SCiTxTrX,chr(34))) {
					((TX_t:=unEnquote(SCiTxTrX))? (action:="unquote", EndAppend:= -2))
				} else {
					sendmessage,2142,SelStart,0,,ahk_id %cWnd%	;SETSELECTIONSTART:=2142;
					sendmessage,2144,SelEnd,0,,ahk_id %cWnd% 	;SETSELECTIONEND:=2144;
					StringTrimleft,SCiTxTrX,SCiTxTrX,1
					StringTrimright,SCiTxTrX,SCiTxTrX,1
					((TX_t:= Enquote(SCiTxTrX))? EndAppend:= 2)
				}
			case,	"Not"	: switch,SCiTxTrX {
					case,	"true"	: TX_t:= "False", EndAppend:= 1
					case,	"false"	: TX_t:= "True", EndAppend:= -1
					case,	"black"	: TX_t:= "White"
					case, "master"	: TX_t:= "Slave"
					case, 	"Slave"	: TX_t:= "Master"
					case,	"1"		: TX_t:= "0"
					case,	"0"		: TX_t:= "1"
					default			: Notice:= True
				} if(Notice) {
					(isHex:=instr(SCiTxTrX,"0x")? (EndAppend:= strlen(TX_t:= FormatDec(SCiTxTrX)) -strlen(SCiTxTrX) )
					: (isInt(SCiTxTrX)? (EndAppend:=strlen(TX_t:= FormatHex(SCiTxTrX))-strlen(SCiTxTrX)) : NotFound:=True))
					Notice:= False
					if(NotFound) {
						sendmessage,2143,0,0,,ahk_id %cWnd% 
						sendmessage,2190,% SelStart:= Errorlevel,0,,ahk_id %cWnd% ;SETTARGETSTART:=2190
						sendmessage,2160,SelStart,SelStart,,ahk_id %cWnd%			;SETSEL:=2160
						return,0
					}
				}
		}
		sendmessage,2143,0,0,,ahk_id %cWnd% 
		sendmessage,2190,% SelStart:= Errorlevel,0,,ahk_id %cWnd% ;SETTARGETSTART:=2190
		sendmessage,2145,0,0,,ahk_id %cWnd% 
		sendmessage,2192,% SelEnd:= Errorlevel,0,,ahk_id %cWnd%  ;SETTARGETEND:=2192
		sendmessage,2160,SelStart,SelEnd,,ahk_id %cWnd%			;SETSEL:=2160
		if(!(len2:= SelEnd-SelStart)>0)
			return,0
		len2++	;considering null-term;
		VarSetCapacity(vAlloxStr,(len2)*2,0)
		, StrPut(TX_t,&vAlloxStr,"UTF-8")
		, vAlloxAddress:= DllCall("VirtualAllocEx","Ptr",(hProc:= DllCall("OpenProcess","UInt",0x438,"Int",False,"UInt",PID
		,"Ptr")),"Ptr",0,"UPtr",len2,"UInt",0x1000,"UInt",4,"Ptr")
		, success:= DllCall("WriteProcessMemory","Ptr",hProc,"Ptr",vAlloxAddress,"Ptr",&vAlloxStr,"Uint",len2+1,"UInt*","","Int")
		EndAppend? SelEnd+=EndAppend:()
		StartAppend? SelStart+=StartAppend:()
		sendmessage,2170,0,vAlloxAddress,,ahk_id %cWnd%	;REPLACESEL:=2170;
		(action="unquote")? ((e!=1||f)? (SelStart++, SelEnd--):()):()
		sendmessage,2142,SelStart,0,,ahk_id %cWnd%		;(SETSELECTIONSTART):=2142;
		sendmessage,2144,SelEnd,0,,ahk_id %cWnd%		;SETSELECTIONEND:=2144;  
		DllCall("VirtualFreeEx","Ptr",hProc,"Ptr",vAlloxAddress,"UPtr",0,"UInt",0x8000) ; MEM_RELEASE
		, DllCall("VirtualFreeEx","Ptr",hProc,"Ptr",Sci_SelTxtPTR,"UPtr",0,"UInt",0x8000)!
		,  DllCall("CloseHandle","Ptr",hProc)
							return,Ret:= (NotFound? 0 : 1)

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
	return,1
	}
}

Byte2Str(Bytes_VarName="",len="",CodePg="CP936") {
	static CP:="CP936"
	(CodePg!="CP936"? CP:= CodePg)
	return,ret:= strget(&(%Bytes_VarName%),len,CP)
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
 return,RegexReplace(regexreplace(target,"(\n?)(.+)","$1;$2"),"(;[\s]*[;]+)","")
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
 return,Regexreplace(target,"(.*[\n\r]*)", surround("$1","quotes")) ;chr(34) "$1" chr(34
}

UnEnquote(target="") { ; ="(?:^)(" Chr(34) ")|(?:^.)(" chr(34) ")|(" chr(34) ")(?:$)|(" chr(34) ")(?:.$)"
 return,RegExReplace(target,chr(34),"") ;this func not required
}

Capitalise(target="") { ;(\b\w)(.*?)||(s(?=cript))
 return,Regexreplace(target,"(_?\w)|(\b\w)(?:.*?)","$U1L2")
}

CapitaliseWithWords(Target="") { ;always capitalised;
	Static Global  Xlist := "AHK,AutoHotKey,Var,Obj,Replace,Get,App_end,Invert,Byte,Hex,Replace,Regex,Format,Exit,String,Target,StrLen" 
	Repl:= Found:= ""
	try,if(S:= RegExReplace(lowercase(target) ,"(\b\w)(?:.*?)|((_\w)|(\w_))","$U1$2"))
		Loop,Parse,% XList,`, ;Parse exceptions;
			if(instr(target,A_loopfield)) {
				Repl:= regexreplace(Repl?Repl:S,"i)(" . SubStr(a_Loopfield, 1 , 1) . ")(" .  (XL_:=SubStr(A_LoopField,2,StrLen(A_LoopField)))  . ")",A_loopfield)
				Found:= True
			} (!Found?  Repl:= S)
	return,Repl
}

FormatHex(Str_in="") {
	return,ret:=(Str_in="")?"0x" : Format("{:#X}",Str_in)
}

FormatDec(Str_in="") {
	return,Format("{:d}",Str_in)
}