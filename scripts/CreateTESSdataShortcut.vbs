Option Explicit

Function AddTrailingBackslash(S)
  if S <> "" Then
    If Right(S, 1) <> "\" Then S = S & "\"
  End If
  AddTrailingBackslash = S
End Function

Function GetWorkDir
  Dim FileSys
  Set FileSys = CreateObject("Scripting.FileSystemObject")
  GetWorkDir = AddTrailingBackslash(FileSys.GetParentFolderName(WScript.ScriptFullName))
End Function

Sub Main
  Dim Shell, Link, DesktopPath, MenuPath, WorkDir
  Set Shell = CreateObject("WScript.Shell")
  WorkDir = GetWorkDir
  DesktopPath = AddTrailingBackslash(Shell.SpecialFolders("Desktop"))
  Set Link = Shell.CreateShortcut(DesktopPath & "TESS LC downloader.lnk")
  Link.TargetPath = WorkDir & "TESSdata.py"
  Link.WorkingDirectory = "%HOMEPATH%"
  Link.Save
  MenuPath = AddTrailingBackslash(Shell.SpecialFolders("Programs"))
  MsgBox "Desktop shortcut have been created successfully!"
End Sub

Main
