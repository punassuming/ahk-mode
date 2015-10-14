; ahklint - a lint program for autohotkey
; Copyright 2012 Dorian Alexander Patterson.
;
; This program is under public domain. If you need to use the program in a 
; jursidiction that does not recoginize the concept of public domain, use the
; MIT License.

#NoEnv
#Warn
FileEncoding, UTF-8-RAW
FileName = %1%
FileContents := []
Report := {}

; Read file contents
Loop, Read, %FileName%
{
	FileContents.Insert(A_LoopReadLine)
}

; Module 1 - Hotstrings
; Hostrings that do not require an ending character trigger, denoted by ":*:" cannot not have matching pattern names, unless #Hostring *0 is specified.
; i.e:
; :*:abc::string1
; :*:abcd::string2
Hotstrings := []
For Index, Value in FileContents
{
	If(SubStr(Value, 1, 2) == ":*")
	{
		;Report["Hotstring"] := 1
		Report[Index] := "This hotstring does not use a ending character trigger. If '#Hotstring *0' is not specified, there is a risk of conflicting hotstrings."
	}
}

ReportString := ""
For Key, Value in Report
{
	ReportString := ReportString . Key . ": " Value . "`n"
}

MsgBox, %ReportString%
