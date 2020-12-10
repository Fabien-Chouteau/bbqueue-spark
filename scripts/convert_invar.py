import fileinput
import re

print("pragma Assert (")
for line in fileinput.input():
    line = line.strip()
    line = line.replace("(Buffer", "(This")
    line = line.replace("Buf'Last", "This.Buf'Last")
    line = line.replace("Read)", "This.Read)")
    line = line.replace("Write)", "This.Write)")
    line = line.replace("(Last", "(This.Last")
    line = line.replace("Reserve", "This.Reserve")
    line = line.replace("Write_In_Progress", "This.Write_In_Progress")
    line = line.replace("Read_In_Progress", "This.Read_In_Progress")
    line = line.replace("Granted_Write_Size", "This.Granted_Write_Size")
    line = line.replace("Granted_Read_Size", "This.Granted_Read_Size")

    print(line);
print(");")
