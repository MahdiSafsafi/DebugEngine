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
// The Original Code is AVX512IFMA-VL.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512IFMA_VL_VPMADD52LUQ_Vn_Kw_z_Hn_B64_Wn
  (PInst: PInstruction);
begin
  // VPMADD52LUQ  Vn{Kw}{z},Hn,B64(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512IFMA or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VPMADD52LUQ;
  Decode_Vn_Kw_z_Hn_B64_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMADD52LUQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512IFMA_VL_VPMADD52HUQ_Vn_Kw_z_Hn_B64_Wn
  (PInst: PInstruction);
begin
  // VPMADD52HUQ  Vn{Kw}{z},Hn,B64(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512IFMA or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VPMADD52HUQ;
  Decode_Vn_Kw_z_Hn_B64_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMADD52HUQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
