# TxtSelReplace
RegEx case substitutions and surrounds within Text selections (Scintilla, Rich Text, Comctl Edit)
Some words can be togglable to alternate words defined in a dictionary array when calling this function with the argument "not"
in principle simillar to a bitwise "!" declaration.
Word Inversion pairs can be added via definiing the comma seperated string "Word_Inversion_Dictionary_CSVSTR" from the caller script.
"Light,Dark,Hot,Cold" and so forth.
todo: rectangular selections, multi selections

use:
loop,parse,% "+,^,^!,,^+,!",`,
	hotkey,% a_loopfield "Capslock",HKi_Handl4,on
loop,parse,% "^;,+2,^+2,+5,+9,+0,[,],+[,+]",`,
	hotkey,% a_loopfield,HKi_Handl4,on
return,
  
HKi_Handl4() {
	Switch,a_thishotkey {
			case, "CapsLock" : txtselreplace("invert")
			case,"^CapsLock" : txtselreplace("lower")
			case,"+Capslock" : txtselreplace("upper")
			;case,"^+CapsLock": txtselreplace("capitalize")	;	Normal	;
			case,"^+CapsLock": txtselreplace("CapitalizeWithWords")	;	Normal	;
			case,"^!CapsLock": send,{CapsLock}
			case,"^;" : txtselreplace("commentline")
			case,"+2" : if !txtselreplace("quote")
					sendinput,% a_thishotkey
			case,"+9","+0" : if !txtselreplace("enclose_brackets")
					sendinput,% a_thishotkey
			case,"+5" : if !txtselreplace("Enclose_Percents")
					sendinput,% a_thishotkey
			case,"+[","+]" : if !txtselreplace("Enclose_braces")
					sendinput,% a_thishotkey
			case,"[","]" : if !txtselreplace("Enclose_square_brackets")
					sendinput,% a_thishotkey
}	}
