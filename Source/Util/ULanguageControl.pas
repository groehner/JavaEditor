unit ULanguageControl;

{gnugettext: scan-all}
interface

var
  _SmkcShift,       // Shift+
  _SmkcCtrl,        // Ctrl+
  _SmkcAlt: string; // Alt+

implementation

uses
  Windows, Messages, Vcl.Consts, Vcl.Menus, SysUtils, Classes, IniFiles,
  Registry, JvGnugettext, UUtils;

var
  _SmkcBkSp,
  _SmkcTab,
  _SmkcEsc,
  _SmkcEnter,
  _SmkcSpace,
  _SmkcPgUp,
  _SmkcPgDn,
  _SmkcEnd,
  _SmkcHome,
  _SmkcLeft,
  _SmkcUp,
  _SmkcRight,
  _SmkcDown,
  _SmkcIns,
  _SmkcDel,

  _SOKButton,
  _SCancelButton,
  _SYesButton,
  _SNoButton,
  _SHelpButton,
  _SCloseButton,
  _SIgnoreButton,
  _SRetryButton,
  _SAbortButton,
  _SAllButton,

  _SMsgDlgWarning,
  _SMsgDlgError,
  _SMsgDlgInformation,
  _SMsgDlgConfirm,
  _SMsgDlgYes,
  _SMsgDlgNo,
  _SMsgDlgOK,
  _SMsgDlgCancel,
  _SMsgDlgHelp,
  _SMsgDlgHelpNone,
  _SMsgDlgHelpHelp,
  _SMsgDlgAbort,
  _SMsgDlgRetry,
  _SMsgDlgIgnore,
  _SMsgDlgAll,
  _SMsgDlgNoToAll,
  _SMsgDlgYesToAll
  : string;


  _SColorBoxCustomCaption,
  _Black,
  _Maroon,
  _Green,
  _Olive,
  _Navy,
  _Purple,
  _Teal,
  _Gray,
  _Silver,
  _Red,
  _Lime,
  _Yellow,
  _Blue,
  _Fuchsia,
  _Aqua,
  _White,
  _MoneyGreen,
  _SkyBlue,
  _Cream,
  _MedGray
  : string;

  PortAppDrive: string;
{
resourcestring
  clNameBlack = 'Schwarz';
  clNameMaroon = 'Braun';
  clNameGreen = 'Grün';
  clNameOlive = 'Olivgrün';
  clNameNavy = 'Dunkelblau';
  clNamePurple = 'Purpur';
  clNameTeal = 'Grünblau';
  clNameGray = 'Grau';
  clNameSilver = 'Silber';
  clNameRed = 'Rot';
  clNameLime = 'Limone';
  clNameYellow = 'Gelb';
  clNameBlue = 'Blau';
  clNameFuchsia = 'Fuchsie';
  clNameAqua = 'Aquamarin';
  clNameWhite = 'Weiß';
  clNameMoneyGreen = 'Dollargrün';
  clNameSkyBlue = 'Himmelblau';
  clNameCream = 'Creme';
  clNameMedGray = 'Mittelgrau';
  clNameActiveBorder = 'Aktiver Rahmen';
  clNameActiveCaption = 'Aktive Titelleiste';
  clNameAppWorkSpace = 'Arbeitsbereich';
  clNameBackground = 'Hintergrund';
  clNameBtnFace = 'Schalterfläche';
  clNameBtnHighlight = 'Schalterhervorhebung';
  clNameBtnShadow = 'Schalterschatten';
  clNameBtnText = 'Schaltertext';
  clNameCaptionText = 'Titeltext';
  clNameDefault = 'Vorgabe';
  clNameGradientActiveCaption = 'Gradient aktiver Titel';
  clNameGradientInactiveCaption = 'Gradient inaktiver Titel';
  clNameGrayText = 'Grauer Text';
  clNameHighlight = 'Hervorgehobener Hintergrund';
  clNameHighlightText = 'Hervorgehobener Text';
  clNameHotLight = 'Hell';
  clNameInactiveBorder = 'Inaktiver Rahmen';
  clNameInactiveCaption = 'Inaktiver Titel';
  clNameInactiveCaptionText = 'Inaktiver Titeltext';
  clNameInfoBk = 'Info-Hintergrund';
  clNameInfoText = 'Info-Text';
  clNameMenu = 'Menühintergrund';
  clNameMenuBar = 'Menüleiste';
  clNameMenuHighlight = 'Markiertes Menü';
  clNameMenuText = 'Menütext';
  clNameNone = 'Ohne';
  clNameScrollBar = 'Bildlaufleiste';
  clName3DDkShadow = '3D Dunkler Schatten';
  clName3DLight = '3D Hell';
  clNameWindow = 'Fensterhintergrund';
  clNameWindowFrame = 'Fensterrahmen';
  clNameWindowText = 'Fenstertext';
  }

procedure ReplaceResourceString(RStringRec: PResStringRec; const AString: string);
  var OldProtect: Cardinal;
begin
  if (RStringRec = nil) or (Length(AString) = 0) then Exit;
  if VirtualProtect(RStringRec, SizeOf(RStringRec^), PAGE_EXECUTE_READWRITE, @OldProtect) then begin
    RStringRec^.Identifier:= Integer(PChar(AString));
    VirtualProtect(RStringRec, SizeOf(RStringRec^), OldProtect, @OldProtect);
  end
  else ErrorMsg(SysErrorMessage(GetLastError));
end;

function PortApp(const Str: string): string;
  var SL1, SL2: TStringList; Int, j: Integer;
begin
  if Copy(Str, 1, 2) = ':\' then // old portable concept
    if Length(PortAppDrive) = 1
      then Result:= PortAppDrive + Str
      else Result:= PortAppDrive + Copy(Str, 2, Length(Str))
  else if Copy(Str, 2, 2) = ':\' then // absolute path
    Result:= Str
  else if Pos('\\', Str) = 1 then   // UNC
    Result:= Str
  else begin // new portable concept, relative to Editorfolder
    SL1:= Split('\', withoutTrailingSlash(ExtractFilePath(ParamStr(0))));
    SL2:= Split('\', withoutTrailingSlash(Str));
    Int:= 0;
    while (Int < SL2.Count) and (SL2.Strings[Int] = '..') do
      Inc(Int);
    Result:= '';
    for j:= 0 to SL1.Count - 1 - Int do
      Result:= Result + SL1.Strings[j] + '\';
    for j:= Int to SL2.Count - 1 do
      Result:= Result + SL2.Strings[j] + '\';
    FreeAndNil(SL1);
    FreeAndNil(SL2);
  end;
end;

function GetLanguageCode: string;
  var Str, language: string;
      MachineIniFile: TMemIniFile;
      MyRegistry: TRegistry;
begin
  Str:= ParamStr(1);
  if (Str = '') or (UpperCase(ExtractFileExt(Str)) <> '.INI')
    then Str:= ExtractFilePath(ParamStr(0)) + 'JEMachine.ini'
    else Str:= ExpandFileName(Str);
  PortAppDrive:= ExtractFileDrive(Str);   // with UNC we get \\Server\Folder
  if Pos(':', PortAppDrive) > 0 then
    PortAppDrive:= Copy(PortAppDrive, 1, 1);
  if FileExists(Str) then begin // INI-File configuration
    MachineIniFile:= TMemIniFile.Create(Str);
    language:= MachineIniFile.ReadString('Options', 'Language', 'XXX');
    Str:= MachineIniFile.Readstring('User', 'HomeDir', '<nix>');
    FreeAndNil(MachineIniFile);

    if language = 'XXX' then begin
      Str:= PortApp(WithTrailingSlash(dissolveUsername(Str))) + 'JEUser.ini';
      MachineIniFile:= TMemIniFile.Create(Str);
      language:= MachineIniFile.ReadString('Options', 'Language', 'XXX');
      if language = 'XXX' then language:= MachineIniFile.ReadString('Java', 'Language', 'XXX');
      if language = 'XXX' then language:= 'en';
      FreeAndNil(MachineIniFile);
    end;
  end else begin
    MyRegistry:= TRegistry.Create;
    with MyRegistry do begin
      RootKey:= HKEY_CURRENT_USER;
      Access:= KEY_READ;
      try
        OpenKey('\Software\JavaEditor\Options' , False);
        if ValueExists('Language')
          then language:= ReadString('Language')
          else language:= '';
      finally
        CloseKey;
      end;
    end;
    MyRegistry.Destroy;
  end;
  if language = '' then
    language:= 'en';
  Result:= language;
end;


initialization

  // GNU Initialization is put here to make sure that
  // all localized strngs get translated
  UseLanguage('');
  UseLanguage(getLanguageCode);

{
  _SmkcShift:= 'Umsch+';       // Shift+
  _SmkcCtrl:= 'Strg+';         // Ctrl+
  _SmkcAlt:= 'Alt+';           // Alt+
}

  _SmkcShift:= _('Shift+');
  _SmkcCtrl:= _('Ctrl+');
  _SmkcAlt:= _('Alt+');

    {
  _SmkcBkSp:= _('SmkcBkSp');
  _SmkcTab:= _('SmkcTab');
  _SmkcEsc:= _('SmkcEsc');
  _SmkcEnter:= _('SmkcEnter');
  _SmkcSpace:= _('SmkcSpace');
  _SmkcPgUp:= _('SmkcPgUp');
  _SmkcPgDn:= _('SmkcPgDn');
  _SmkcEnd:= _('SmkcEnd');
  _SmkcHome:= _('SmkcHome');
  _SmkcLeft:= _('SmkcLeft');
  _SmkcUp:= _('SmkcUp');
  _SmkcRight:= _('SmkcRight');
  _SmkcDown:= _('SmkcDown');
  _SmkcIns:= _('SmkcIns');
  _SmkcDel:= _('SmkcDel');

  _SOKButton:= _('SOKButton');
  _SCancelButton:= _('SCancelButton');
  _SYesButton:= _('SYesButton');
  _SNoButton:= _('SNoButton');
  _SHelpButton:= _('SHelpButton');
  _SCloseButton:= _('SCloseButton');
  _SIgnoreButton:= _('SIgnoreButton');
  _SRetryButton:= _('SRetryButton');
  _SAbortButton:= _('SAbortButton');
  _SAllButton:= _('SAllButton');

  _SMsgDlgWarning:= _('SMsgDlgWarning');
  _SMsgDlgError:= _('SMsgDlgError');
  _SMsgDlgInformation:= _('SMsgDlgInformation');
  _SMsgDlgConfirm:= _('SMsgDlgConfirm');
  _SMsgDlgYes:= _('SMsgDlgYes');
  _SMsgDlgNo:= _('SMsgDlgNo');
  _SMsgDlgOK:= _('SMsgDlgOK');
  _SMsgDlgCancel:= _('SMsgDlgCancel');
  _SMsgDlgHelp:= _('SMsgDlgHelp');
  _SMsgDlgHelpNone:= _('SMsgDlgHelpNone');
  _SMsgDlgHelpHelp:= _('SMsgDlgHelpHelp');
  _SMsgDlgAbort:= _('SMsgDlgAbort');
  _SMsgDlgRetry:= _('SMsgDlgRetry');
  _SMsgDlgIgnore:= _('SMsgDlgIgnore');
  _SMsgDlgAll:= _('SMsgDlgAll');
  _SMsgDlgNoToAll:= _('SMsgDlgNoToAll');
  _SMsgDlgYesToAll:= _('SMsgDlgYesToAll');

  _SColorBoxCustomCaption:= _('SColorBoxCustomCaption');
  _Black:= _('Black');
  _Maroon:= _('Maroon');
  _Green:= _('Green');
  _Olive:= _('Olive');
  _Navy:= _('Navy');
  _Purple:= _('Purple');
  _Teal:= _('Teal');
  _Gray:= _('Gray');
  _Silver:= _('Silver');
  _Red:= _('Red');
  _Lime:= _('Lime');
  _Yellow:= _('Yellow');
  _Blue:= _('Blue');
  _Fuchsia:= _('Fuchsia');
  _Aqua:= _('Aqua');
  _White:= _('White');
  _MoneyGreen:= _('MoneyGreen');
  _SkyBlue:= _('SkyBlue');
  _Cream:= _('Cream');
  _MedGray:= _('MedGray');
      }

    {
    MenuKeyCaps[mkcBkSp]:= _SmkcBkSp;
    MenuKeyCaps[mkcTab]:= _SmkcTab;
    MenuKeyCaps[mkcEsc]:= _SmkcEsc;
    MenuKeyCaps[mkcEnter]:= _SmkcEnter;
    MenuKeyCaps[mkcSpace]:= _SmkcSpace;
    MenuKeyCaps[mkcPgUp]:= _SmkcPgUp;
    MenuKeyCaps[mkcPgDn]:= _SmkcPgDn;
    MenuKeyCaps[mkcEnd]:= _SmkcEnd;
    MenuKeyCaps[mkcHome]:= _SmkcHome;
    MenuKeyCaps[mkcLeft]:= _SmkcLeft;
    MenuKeyCaps[mkcUp]:= _SmkcUp;
    MenuKeyCaps[mkcRight]:= _SmkcRight;
    MenuKeyCaps[mkcDown]:= _SmkcDown;
    MenuKeyCaps[mkcIns]:= _SmkcIns;
    MenuKeyCaps[mkcDel]:= _SmkcDel;
    MenuKeyCaps[mkcShift]:= _SmkcShift;
    MenuKeyCaps[mkcCtrl]:= _SmkcCtrl;
    MenuKeyCaps[mkcAlt]:= _SmkcAlt;
    }

    // that is really working!
    ReplaceResourceString(@SmkcBkSp, _SmkcBkSp);
    ReplaceResourceString(@SmkcTab, _SmkcTab);
    ReplaceResourceString(@SmkcEsc, _SmkcEsc);
    ReplaceResourceString(@SmkcEnter, _SmkcEnter);
    ReplaceResourceString(@SmkcSpace, _SmkcSpace);
    ReplaceResourceString(@SmkcPgUp, _SmkcPgUp);
    ReplaceResourceString(@SmkcPgDn, _SmkcPgDn);
    ReplaceResourceString(@SmkcEnd, _SmkcEnd);
    ReplaceResourceString(@SmkcHome, _SmkcHome);
    ReplaceResourceString(@SmkcLeft, _SmkcLeft);
    ReplaceResourceString(@SmkcUp, _SmkcUp);
    ReplaceResourceString(@SmkcRight, _SmkcRight);
    ReplaceResourceString(@SmkcDown, _SmkcDown);
    ReplaceResourceString(@SmkcIns, _SmkcIns);
    ReplaceResourceString(@SmkcDel, _SmkcDel);
    ReplaceResourceString(@SmkcShift, _SmkcShift);
    ReplaceResourceString(@SmkcCtrl, _SmkcCtrl);
    ReplaceResourceString(@SmkcAlt, _SmkcAlt);

    ReplaceResourceString(@SOKButton, _SOKButton);
    ReplaceResourceString(@SCancelButton, _SCancelButton);
    ReplaceResourceString(@SYesButton, _SYesButton);
    ReplaceResourceString(@SNoButton, _SNoButton);
    ReplaceResourceString(@SHelpButton, _SHelpButton);
    ReplaceResourceString(@SCloseButton, _SCloseButton);
    ReplaceResourceString(@SIgnoreButton, _SIgnoreButton);
    ReplaceResourceString(@SRetryButton, _SRetryButton);
    ReplaceResourceString(@SAbortButton, _SAbortButton);
    ReplaceResourceString(@SAllButton, _SAllButton);
    ReplaceResourceString(@SMsgDlgWarning, _SMsgDlgWarning);
    ReplaceResourceString(@SMsgDlgError, _SMsgDlgError);
    ReplaceResourceString(@SMsgDlgInformation, _SMsgDlgInformation);
    ReplaceResourceString(@SMsgDlgConfirm, _SMsgDlgConfirm);
    ReplaceResourceString(@SMsgDlgYes, _SMsgDlgYes);
    ReplaceResourceString(@SMsgDlgNo, _SMsgDlgNo);
    ReplaceResourceString(@SMsgDlgOK, _SMsgDlgOK);
    ReplaceResourceString(@SMsgDlgCancel, _SMsgDlgCancel);
    ReplaceResourceString(@SMsgDlgHelp, _SMsgDlgHelp);
    ReplaceResourceString(@SMsgDlgHelpNone, _SMsgDlgHelpNone);
    ReplaceResourceString(@SMsgDlgHelpHelp, _SMsgDlgHelpHelp);
    ReplaceResourceString(@SMsgDlgAbort, _SMsgDlgAbort);
    ReplaceResourceString(@SMsgDlgRetry, _SMsgDlgRetry);
    ReplaceResourceString(@SMsgDlgIgnore, _SMsgDlgIgnore);
    ReplaceResourceString(@SMsgDlgAll, _SMsgDlgAll);
    ReplaceResourceString(@SMsgDlgNoToAll, _SMsgDlgNoToAll);
    ReplaceResourceString(@SMsgDlgYesToAll, _SMsgDlgYesToAll);
    ReplaceResourceString(@SColorBoxCustomCaption, _SColorBoxCustomCaption);

{    ReplaceResourceString(@clNameBlack, _Black);
    ReplaceResourceString(@clNameMaroon, _Maroon);
    ReplaceResourceString(@clNameGreen, _Green);
    ReplaceResourceString(@clNameOlive, _Olive);
    ReplaceResourceString(@clNameNavy, _Navy);
    ReplaceResourceString(@clNamePurple, _Purple);
    ReplaceResourceString(@clNameTeal, _Teal);
    ReplaceResourceString(@clNameGray, _Gray);
    ReplaceResourceString(@clNameSilver, _Silver);
    ReplaceResourceString(@clNameRed, _Red);
    ReplaceResourceString(@clNameLime, _Lime);
    ReplaceResourceString(@clNameYellow, _Yellow);
    ReplaceResourceString(@clNameBlue, _Blue);
    ReplaceResourceString(@clNameFuchsia, _Fuchsia);
    ReplaceResourceString(@clNameAqua, _Aqua);
    ReplaceResourceString(@clNameWhite, _White);
    ReplaceResourceString(@clNameMoneyGreen, _MoneyGreen);
    ReplaceResourceString(@clNameSkyBlue, _SkyBlue);
    ReplaceResourceString(@clNameCream, _Cream);
    ReplaceResourceString(@clNameMedGray, _MedGray);
    }

end.

