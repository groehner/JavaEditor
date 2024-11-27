(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower ShellShock
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ShellShock: SsDefine.inc 1.02                         *}
{*********************************************************}
{* ShellShock: Compiler options/directives include file  *}
{*********************************************************}

{ Because StDefine.inc is included in all ShellShock units, you can specify }
{ global compiler options here. STDEFINE is included *before* each units'   }
{ required compiler options, so options specified here might be overridden  }
{ by hardcoded options in the source file.                                  }

{$A+} {Word Align Data}
{$H+} {Huge string support}
{$Q-} {Overflow check}
{$R-} {Range check}
{$S-} {Stack check}
{$T-} {Typed @ check}
{$V-} {Var strings}
{$B-} {Incomplete boolean evaluation}
{$J+} {Writeable Typed Constants}

{$DEFINE ThreadSafe}
{ This define determines whether ShellShock operations are safe in a 32-bit }
{ multi-threaded application. Leaving it undefined will improved performance}
{ slightly in a single-threaded 32-bit application.                         }

{.$DEFINE SuppressWarnings}
{ This define enables you to suppress warnings that are generated for code  }
{ in ShellShock by Delphi 2/3. The two most common warnings are (a) function}
{ does not set Result and (b) variable used before being initialized. The   }
{ code however has been tested to be correct but is complex enough to fool  }
{ the compiler. You should leave this define activated.                     }


{===========!! DO NOT ALTER ANYTHING BEYOND THIS POINT !!==================}
{===========!! DO NOT ALTER ANYTHING BEYOND THIS POINT !!==================}


{$IFDEF VER93}
  {$DEFINE CBuilder}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE CBuilder}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE CBuilder}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER130}
  {$IFDEF BCB}
    {$DEFINE CBuilder}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}
{$IFDEF VER140}
  {$IFDEF BCB}
    {$DEFINE CBuilder}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}

{$IFNDEF WIN32}
  {$IFDEF VER80}
    {$DEFINE WIN16} { Delphi 1.0, 16-Bit Windows }
  {$ENDIF}
{$ENDIF}

{$IFNDEF VER80}       {Delphi 1.0}
 {$DEFINE VERSION2}   {Delphi 2.0 and BCB 1 or higher}
{$ENDIF}

{$IFDEF VERSION2}
 {$IFNDEF VER90}      {Delphi 2.0}
  {$IFNDEF VER93}     {BCB 1.0}
   {$DEFINE VERSION3} {Delphi 3.0 or BCB 3.0}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION3}
 {$IFNDEF VER100}     {Delphi 3}
  {$IFNDEF VER110}    {BCB 3}
   {$DEFINE VERSION4} {Delphi 4.0 or higher}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION4}
 {$IFNDEF VER120}        { Delphi 4 }
  {$IFNDEF VER125}       { BCB 4 }
      {$DEFINE VERSION5} { Delphi 5.0 or higher }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION5}
 {$IFNDEF VER130}        { Delphi 5 }
  {$IFNDEF VER135}       { BCB 5 }
    {$IFNDEF LINUX }
      {$DEFINE VERSION6} { Delphi 6.0 or higher }
    {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION5}
 {$IFNDEF VER140}        { Delphi 6 }
  {$IFNDEF LINUX }
    {$DEFINE VERSION7} { Delphi 7.0 or higher }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION7}
  {$IF compilerversion >= 20}     // Delphi 2009 or higher
    {$DEFINE VERSION2009}
  {$IFEND}
  {$IF compilerversion >= 21}     // Delphi 2010 or higher
    {$DEFINE VERSION2010}
  {$IFEND}
  {$IF compilerversion >= 22}     // Delphi XE or higher
    {$DEFINE VERSIONXE}
  {$IFEND}
  {$IF compilerversion >= 23}     // Delphi XE2 or higher
    {$DEFINE VERSIONXE2}
  {$IFEND}
{$ENDIF}


{$IFDEF VERSION4}
  {$DEFINE HasLongWord}         { D4 and above have a true LongWord type }
  {$DEFINE HasInt64 }           { D4 and above have a 64-bit Integer }
{$ENDIF}

{$IFDEF VERSION2}
 {$IFNDEF VERSION3}
  {$DEFINE VERSION2ONLY}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION3}
 {$IFNDEF VERSION4}
  {$DEFINE VERSION3ONLY}
 {$ENDIF}
{$ENDIF}

{Set up the string type expected}
{$UNDEF HStrings}
{$UNDEF WStrings}
{$DEFINE NStrings}     {255-character strings under Win16}
{$IFOPT H+}
  {$DEFINE HStrings} {Huge strings under WIN32}
  {$UNDEF NStrings}
{$ELSE}
  {$DEFINE WStrings} {255-character strings under WIN32}
  {$UNDEF NStrings}
{$ENDIF}


