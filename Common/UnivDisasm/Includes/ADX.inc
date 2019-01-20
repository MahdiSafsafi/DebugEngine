//
// *************************************************************************** //
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
// *************************************************************************** //
//
//
// *************************************************************************** //
// UnivDisasm library.
//
// This file is a part of UnivDisasm library.
//
// https://github.com/MahdiSafsafi/UnivDisasm
//
// The Original Code is ADX.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_ADX_ADCX_Gy_Ey(PInst: PInstruction);
begin
  // ADCX  Gy,Ey
  PInst^.InstGroups := INST_GRP_ADX;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_ADCX;
  Decode_Gy_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADCX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_ADX_ADOX_Gy_Ey(PInst: PInstruction);
begin
  // ADOX  Gy,Ey
  PInst^.InstGroups := INST_GRP_ADX;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_ADOX;
  Decode_Gy_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADOX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
