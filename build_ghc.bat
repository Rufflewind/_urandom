@if (@a==@b) @end /*

@rem  This is a batch script for building GHC on Windows 7 or later.  You will
@rem  need Internet access and a generous amount of patience.  Run the script
@rem  inside an empty directory, as it will create two subdirectories: "ghc"
@rem  and sometimes "stack" (if Stack is not already installed).
@rem
@rem  The script prepares the build environment (if necessary), acquires the
@rem  source code (if necessary), and then starts the build.
@rem
@rem  The main purpose of the script is to set up the environment for building
@rem  GHC.  Once you have used it at least once, you can simply start a shell
@rem  using "stack exec --no-ghc-package-path mintty" and then follow the usual
@rem  build instructions at:
@rem
@rem      https://ghc.haskell.org/trac/ghc/wiki/Building/QuickStart

@setlocal

@rem  query architecture
@reg Query HKLM\Hardware\Description\System\CentralProcessor\0 ^
    | find /i "x86" >NUL && set arch=i386 || set arch=x86_64

@where stack >NUL 2>NUL && goto skip_local_stack
@if exist stack goto skip_stack
setlocal
mkdir stack
cd stack

@rem  download stack.zip from website
@set args='https://www.stackage.org/stack/windows-%arch%', 'stack.zip'
powershell -Command "(New-Object Net.WebClient).DownloadFile(%args%)" || (
    cd .. && rmdir /q /s stack
    exit /b
)

@rem  call JScript to extract stack.exe from stack.zip
cscript "%~f0" //Nologo //E:jscript || (
    cd .. && rmdir /q /s stack
    exit /b
)
del stack.zip

endlocal
:skip_stack
set PATH=%CD%\stack;%PATH%
:skip_local_stack

@rem  install GHC, MinGW, and other tools
stack setup || exit /b
stack install alex happy || exit /b

@rem  attempt pacman several times because SourceForge is unreliable
@set cmd=stack exec -- pacman -Syuu --needed --noconfirm ^
    autoconf automake binutils curl git libtool make mingw-w64-%arch%-gcc ^
    mingw-w64-%arch%-python3-sphinx p7zip patch python python2 tar
%cmd% || %cmd% || %cmd% || %cmd% || exit /b
@rem  somehow ca-certificates is broken and needs to be reinstalled
stack exec -- pacman -S --noconfirm ca-certificates

@rem  get the source code
@if exist ghc goto skip_ghc
stack exec -- git clone --recursive https://git.haskell.org/ghc
:skip_ghc

@rem  configure and build the source code
cd ghc
stack exec --no-ghc-package-path -- sh -c ./boot
stack exec --no-ghc-package-path -- sh -c ^
    "./configure --enable-tarballs-autodownload"
stack exec --no-ghc-package-path -- make -j %NUMBER_OF_PROCESSORS%

@rem  (end of script; remaining part is the JScript invoked earlier)
@goto :EOF
*/

/* extract stack.exe from stack.zip */
var fso = new ActiveXObject('Scripting.FileSystemObject');
var shell = new ActiveXObject('Shell.Application');
var zip = shell.NameSpace(fso.getFile("stack.zip").Path);
var dest = shell.NameSpace(fso.getFolder(".").Path);
var items = zip.Items();
var count = items.Count;
for (var i = 0; i < count; ++i) {
    var item = items.Item(i);
    if (item.Name.toLowerCase() == "stack.exe") {
        dest.CopyHere(item, 4 + 16);
        break;
    }
}
