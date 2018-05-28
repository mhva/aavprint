@echo off

rem "-----------------------------------"
rem "SET YOUR USERNAME AND PASSWORD HERE"
rem "-----------------------------------"
set  USERNAME_=""
set  PASSWORD_=""
rem "-----------------------------------"

set /P TERM_STARTS=Start date (e.g. 2014-09-01): %=%
set /P TERM_ENDS=End date (e.g. 2014-11-12)    : %=%
jnl.exe --username="%USERNAME_%" ^
        --password="%PASSWORD_%" ^
        --start-date="%TERM_STARTS%" ^
        --end-date="%TERM_ENDS%" ^
        --report-layout="grid" ^
        --login-url="http://diary-db.kirov.ru/sch61/login" ^
        --api-url="http://diary-db.kirov.ru/sch61/act" ^
        --output="%TEMP%\jnl-breakdown.html"
if %ERRORLEVEL% neq 0 goto failure

:success
echo Opening report in bundled web browser...
FirefoxPortable\FirefoxPortable.exe "file://%TEMP%\jnl-breakdown.html"
pause
exit /B 0

:failure
pause
exit /B 1