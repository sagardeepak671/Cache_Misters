^+v:: ; Ctrl + Shift + V
    filePath := "C:\Users\deepa\Desktop\Cache_Misters\.vscode\a4\l.ml" ; Corrected path with quotes
    FileRead, fileContents, %filePath%
    Clipboard := fileContents
    Send ^v ; Paste the content from the clipboard
return
