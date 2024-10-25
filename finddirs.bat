@echo off
for /f "usebackq" %%i in (`"dir D:\projects /B /A:D"`) do (
    echo D:\projects\%%i
)

for /f "usebackq" %%i in (`"dir D:\work /B /A:D"`) do (
    echo D:\work\%%i
)
