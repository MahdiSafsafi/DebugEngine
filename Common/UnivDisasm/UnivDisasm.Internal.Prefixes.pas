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
// The Original Code is UnivDisasm.Internal.Prefixes.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

unit UnivDisasm.Internal.Prefixes;

interface

{$I Config.inc}

uses UnivDisasm.Disasm;
procedure Decode_PREFIXES_ES_void(PInst: PInstruction);
procedure Decode_PREFIXES_CS_void(PInst: PInstruction);
procedure Decode_PREFIXES_SS_void(PInst: PInstruction);
procedure Decode_PREFIXES_DS_void(PInst: PInstruction);
procedure Decode_PREFIXES_REX_void(PInst: PInstruction);
procedure Decode_PREFIXES_DREX_void(PInst: PInstruction);
procedure Decode_PREFIXES_EVEX_void(PInst: PInstruction);
procedure Decode_PREFIXES_XOP_void(PInst: PInstruction);
procedure Decode_PREFIXES_FS_void(PInst: PInstruction);
procedure Decode_PREFIXES_GS_void(PInst: PInstruction);
procedure Decode_PREFIXES_OPSIZE_void(PInst: PInstruction);
procedure Decode_PREFIXES_ADSIZE_void(PInst: PInstruction);
procedure Decode_PREFIXES_VEX3_void(PInst: PInstruction);
procedure Decode_PREFIXES_VEX2_void(PInst: PInstruction);
procedure Decode_PREFIXES_LOCK_void(PInst: PInstruction);
procedure Decode_PREFIXES_F2_void(PInst: PInstruction);
procedure Decode_PREFIXES_F3_void(PInst: PInstruction);
procedure Decode_PREFIXES_BND_void(PInst: PInstruction);
procedure Decode_PREFIXES_REPNE_void(PInst: PInstruction);
procedure Decode_PREFIXES_XAQUIRE_void(PInst: PInstruction);
procedure Decode_PREFIXES_REP_void(PInst: PInstruction);
procedure Decode_PREFIXES_REPE_void(PInst: PInstruction);
procedure Decode_PREFIXES_XRELEASE_void(PInst: PInstruction);

implementation

uses
  UnivDisasm.Internal.Common,
  UnivDisasm.Cnsts,
  UnivDisasm.Cnsts.Regs;

Procedure JumpToTab2(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_2);
  TABLE_2[PInst.NextInst^](PInst);
end;

Procedure JumpToTab38(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_38);
  TABLE_38[PInst.NextInst^](PInst);
end;

Procedure JumpToTab3A(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_3A);
  TABLE_3A[PInst.NextInst^](PInst);
end;

Procedure JumpInvalid(PInst: PInstruction);
begin

end;

const
  mmmmmToEscProc: array [0 .. 7] of TDecoderProc = ( //
    JumpInvalid, { }
    JumpToTab2, { 00001: implied 0F leading opcode byte }
    JumpToTab38, { 00010: implied 0F 38 leading opcode bytes }
    JumpToTab3A, { 00011: implied 0F 3A leading opcode bytes }
    JumpInvalid, { }
    JumpInvalid, { }
    JumpInvalid, { }
    JumpInvalid { }

    );

  RSel: array [Boolean] of Byte = ($00, $08);
  XSel: array [Boolean] of Byte = ($00, $08);
  BSel: array [Boolean] of Byte = ($00, $08);
  RpSel: array [Boolean] of Byte = ($00, $10);
  VpSel: array [Boolean] of Byte = ($00, $10);

  L2VL: array [0 .. 3] of Byte = (SIZE_OWORD, SIZE_YWORD, SIZE_ZWORD, 0);
  L2Rnd: array [0 .. 3] of Byte = (RN_SAE, RD_SAE, RU_SAE, RZ_SAE);

  PP2MndPrf: array [0 .. 3] of Byte = (MND_PRF_NA, MND_PRF_66, MND_PRF_F3, MND_PRF_F2);
  CPUX2SegFlags: array [0 .. 1] of Byte = (PF_USED or PF_VALID, PF_USED);

procedure ClearPrfNA(PInst: PInstruction);
begin
  PInst^.InternalData.MandatoryPrefixes := PInst^.InternalData.MandatoryPrefixes and not MND_PRF_NA;
end;

procedure UpdateOpSizeW(PInst: PInstruction);
begin
  if PInst^.Fields.W then
  begin
    PInst^.InternalData.OpSizeY := SIZE_QWORD;
    PInst^.InternalData.OpSizeV := SIZE_QWORD;
    PInst^.InternalData.OpSizeZ := SIZE_DWORD;
  end
  else
    PInst^.InternalData.OpSizeY := SIZE_DWORD;
end;

procedure Decode_PREFIXES_ES_void(PInst: PInstruction);
begin
  PInst^.Prefixes.SegESPrf.Flags := CPUX2SegFlags[PInst^.Arch];
  if PInst^.Arch = CPUX64 then
    PInst^.Warn(WARN_ES_PREFIX_IGNORED);
  PInst^.InternalData.Seg := SEG_ES;
  if PInst^.Prefixes.SegESPrf.Count > 0 then
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.SegESPrf);
    // PInst^.Warn(WARN_SUPERFLUOUS_ES_PREFIX);
  end;
  Inc(PInst^.Prefixes.SegESPrf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_CS_void(PInst: PInstruction);
begin
  PInst^.Prefixes.SegCSPrf.Flags := CPUX2SegFlags[PInst^.Arch];
  if PInst^.Arch = CPUX64 then
    PInst^.Warn(WARN_CS_PREFIX_IGNORED);
  PInst^.InternalData.Seg := SEG_CS;
  if PInst^.Prefixes.SegCSPrf.Count > 0 then
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.SegCSPrf);
    // PInst^.Warn(WARN_SUPERFLUOUS_CS_PREFIX);
  end;
  Inc(PInst^.Prefixes.SegCSPrf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_SS_void(PInst: PInstruction);
begin
  PInst^.Prefixes.SegSSPrf.Flags := CPUX2SegFlags[PInst^.Arch];
  if PInst^.Arch = CPUX64 then
    PInst^.Warn(WARN_SS_PREFIX_IGNORED);
  PInst^.InternalData.Seg := SEG_SS;
  if PInst^.Prefixes.SegSSPrf.Count > 0 then
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.SegSSPrf);
    // PInst^.Warn(WARN_SUPERFLUOUS_SS_PREFIX);
  end;
  Inc(PInst^.Prefixes.SegSSPrf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_DS_void(PInst: PInstruction);
begin
  PInst^.Prefixes.SegDSPrf.Flags := CPUX2SegFlags[PInst^.Arch];
  if PInst^.Arch = CPUX64 then
    PInst^.Warn(WARN_DS_PREFIX_IGNORED);
  PInst^.InternalData.Seg := SEG_DS;
  if PInst^.Prefixes.SegDSPrf.Count > 0 then
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.SegDSPrf);
    // PInst^.Warn(WARN_SUPERFLUOUS_DS_PREFIX);
  end;
  Inc(PInst^.Prefixes.SegDSPrf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_REX_void(PInst: PInstruction);
var
  P: PByte;
begin
  PInst^.Encoding := ENC_REX;
  PInst^.Prefixes.Rex.Flags := PF_USED or PF_VALID;
  P := PInst^.NextInst;
  Dec(P);
  PInst^.Fields.R := RSel[P^ and 4 <> 0];
  PInst^.Fields.X := XSel[P^ and 2 <> 0];
  PInst^.Fields.B := BSel[P^ and 1 <> 0];
  PInst^.Fields.W := P^ and 8 <> 0;
  PInst^.InternalData.MagicRex := MagicRexMask;
  UpdateOpSizeW(PInst);
  if PInst^.Prefixes.Rex.Count > 0 then
    SuperfluousPrefix(PInst, PInst^.Prefixes.Rex);
  Inc(PInst^.Prefixes.Rex.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_DREX_void(PInst: PInstruction);
var
  P: Byte;
begin
  PInst^.Encoding := ENC_DREX;
  PInst^.Prefixes.DRex.Flags := PF_USED or PF_VALID;
  Inc(PInst^.Prefixes.DRex.Count);
  Inc(PInst^.Prefixes.Count);
  P := PInst^.NextInst^;
  PInst^.Fields.R := RSel[P and 4 <> 0];
  PInst^.Fields.X := XSel[P and 2 <> 0];
  PInst^.Fields.B := BSel[P and 1 <> 0];
  PInst^.Fields.ocz0 := P and 8 <> 0;
  PInst^.Fields.Dest := P shr 4;
  UpdateOpSizeW(PInst);
  PInst^.Next();
end;

procedure Decode_PREFIXES_FS_void(PInst: PInstruction);
begin
  PInst^.Prefixes.SegFSPrf.Flags := PF_USED or PF_VALID;
  PInst^.InternalData.Seg := SEG_FS;
  if PInst^.Prefixes.SegFSPrf.Count > 0 then
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.SegFSPrf);
    // PInst^.Warn(WARN_SUPERFLUOUS_FS_PREFIX);
  end;
  Inc(PInst^.Prefixes.SegFSPrf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_GS_void(PInst: PInstruction);
begin
  PInst^.Prefixes.SegGSPrf.Flags := PF_USED or PF_VALID;
  PInst^.InternalData.Seg := SEG_GS;
  if PInst^.Prefixes.SegGSPrf.Count > 0 then
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.SegGSPrf);
    // PInst^.Warn(WARN_SUPERFLUOUS_GS_PREFIX);
  end;
  Inc(PInst^.Prefixes.SegGSPrf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_OPSIZE_void(PInst: PInstruction);
begin
  if PInst^.Prefixes.OpSizePrf.Count = 0 then
  begin
    PInst^.InternalData.MandatoryPrefixes := PInst^.InternalData.MandatoryPrefixes or MND_PRF_66;
    PInst^.Prefixes.OpSizePrf.Flags := PF_USED;
    PInst^.InternalData.OpSizeV := SIZE_WORD;
    PInst^.InternalData.OpSizeZ := SIZE_WORD;
  end
  else
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.OpSizePrf);
    // PInst^.Warn(WARN_SUPERFLUOUS_OPSIZE_PREFIX);
  end;
  Inc(PInst^.Prefixes.OpSizePrf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_ADSIZE_void(PInst: PInstruction);
begin
  if PInst^.Prefixes.AddrSizePrf.Count = 0 then
  begin
    PInst^.Prefixes.AddrSizePrf.Flags := PF_USED;
    PInst^.AddressMode := PInst^.AddressMode - 1;
  end
  else
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.AddrSizePrf);
    // PInst^.Warn(WARN_SUPERFLUOUS_ADDRESS_SIZE_PREFIX);
  end;
  Inc(PInst^.Prefixes.AddrSizePrf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_XOP_void(PInst: PInstruction);
var
  P: Byte;
begin
  PInst^.Encoding := ENC_XOP;
  PInst^.Prefixes.XOPPrf.Flags := PF_USED or PF_VALID;
  Inc(PInst^.Prefixes.XOPPrf.Count);
  Inc(PInst^.Prefixes.Count);
  P := PInst^.NextInst^;
  PInst^.Fields.R := RSel[P and $80 = $00];
  PInst^.Fields.X := XSel[P and $40 = $00];
  PInst^.Fields.B := BSel[P and $20 = $00];
  PInst^.Fields.mmmmm := P and $1F;
  if not PInst^.Next() then
    Exit;
  P := PInst^.NextInst^;
  PInst^.Fields.W := P and $80 <> $00;
  PInst^.Fields.vvvv := $0F - ((P and $78) shr $03);
  PInst^.Fields.PP := PP2MndPrf[P and $03];
  PInst^.InternalData.MandatoryPrefixes := PP2MndPrf[P and $03];
  PInst^.Fields.VL := L2VL[(P and $04) shr $02];
  if not PInst^.Next() then
    Exit;
  UpdateOpSizeW(PInst);
  case PInst^.Fields.mmmmm of
    $08:
      begin
        PInst^.SetTable(GF_TABLE_XOP_8);
        TABLE_XOP8[PInst^.NextInst^](PInst);
      end;
    $09:
      begin
        PInst^.SetTable(GF_TABLE_XOP_9);
        TABLE_XOP9[PInst^.NextInst^](PInst);
      end;
    $0A:
      begin
        PInst^.SetTable(GF_TABLE_XOP_A);
        TABLE_XOPA[PInst^.NextInst^](PInst);
      end;
  else PInst^.Error(ERROR_INVALID_XOP_ESCAPE);
  end;
end;

procedure Decode_PREFIXES_EVEX_void(PInst: PInstruction);
var
  P: Byte;
begin
  PInst^.Prefixes.EVEXPrf.Flags := PF_USED or PF_VALID;
  PInst^.Encoding := ENC_EVEX;
  Inc(PInst^.Prefixes.EVEXPrf.Count);
  Inc(PInst^.Prefixes.Count);
  P := PInst^.NextInst^; // P0 !
  PInst^.Fields.R := RSel[P and $80 = $00];
  PInst^.Fields.X := XSel[P and $40 = $00];
  PInst^.Fields.B := BSel[P and $20 = $00];
  PInst^.Fields.Rp := RpSel[P and $10 = $00];
  PInst^.Fields.mmmmm := P and $03;
  if not PInst^.Next() then
    Exit;
  P := PInst^.NextInst^; // P1 !
  PInst^.Fields.W := P and $80 <> $00;
  PInst^.Fields.vvvv := $0F - ((P and $78) shr $03);
  PInst^.Fields.PP := PP2MndPrf[P and $03];
  PInst^.InternalData.MandatoryPrefixes := PP2MndPrf[P and $03];
  if not PInst^.Next() then
    Exit;
  P := PInst^.NextInst^; // P2 !
  PInst^.Fields.z := P and $80 <> $00;
  PInst^.Fields.VL := L2VL[(P and $60) shr $05];
  PInst^.Fields.bc := P and $10 <> $00;
  if PInst^.Fields.bc then
  begin
    PInst^.Fields.Rnd := L2Rnd[(P and $60) shr $05];
    PInst^.Fields.VL := SIZE_ZWORD;
  end;
  PInst^.Fields.Vp := VpSel[P and $08 = $00];
  PInst^.Fields.aaa := P and $07;
  if not PInst^.Next() then
    Exit;
  UpdateOpSizeW(PInst);
  if (PInst^.Fields.mmmmm = $00) or (PInst^.Fields.mmmmm > $03) then
    PInst^.Error(ERROR_INVALID_EVEX_ESCAPE);
  mmmmmToEscProc[PInst^.Fields.mmmmm and 7](PInst);
end;

procedure Decode_PREFIXES_VEX3_void(PInst: PInstruction);
var
  P: Byte;
begin
  PInst^.Prefixes.VEXPrf.Flags := PF_USED or PF_VALID or PF_VEX3;
  PInst^.Encoding := ENC_VEX;
  Inc(PInst^.Prefixes.VEXPrf.Count);
  Inc(PInst^.Prefixes.Count);
  P := PInst^.NextInst^;
  PInst^.Fields.R := RSel[P and $80 = $00];
  PInst^.Fields.X := XSel[P and $40 = $00];
  PInst^.Fields.B := BSel[P and $20 = $00];
  PInst^.Fields.mmmmm := P and $1F;
  if not PInst^.Next() then
    Exit;
  P := PInst^.NextInst^;
  PInst^.Fields.W := (P and $80 <> 0);
  PInst^.Fields.vvvv := $0F - ((P and $78) shr $03);
  PInst^.Fields.VL := L2VL[(P and $04) shr $02];
  PInst^.Fields.PP := PP2MndPrf[P and $03];
  PInst^.InternalData.MandatoryPrefixes := PP2MndPrf[P and $03];
  if not PInst^.Next() then
    Exit;
  UpdateOpSizeW(PInst);
  if (PInst^.Fields.mmmmm = $00) or (PInst^.Fields.mmmmm > $03) then
    PInst^.Error(ERROR_INVALID_VEX_ESCAPE);
  mmmmmToEscProc[PInst^.Fields.mmmmm and 7](PInst);
end;

procedure Decode_PREFIXES_VEX2_void(PInst: PInstruction);
var
  P: Byte;
begin
  PInst^.Prefixes.VEXPrf.Flags := PF_USED or PF_VALID or PF_VEX2;
  PInst^.Encoding := ENC_VEX;
  Inc(PInst^.Prefixes.VEXPrf.Count);
  Inc(PInst^.Prefixes.Count);
  P := PInst^.NextInst^;
  PInst^.Fields.R := RSel[P and $80 = $00];
  PInst^.Fields.vvvv := $0F - ((P and $78) shr $03);
  PInst^.Fields.VL := L2VL[(P and $04) shr $02];
  PInst^.Fields.PP := PP2MndPrf[P and $03];
  PInst^.InternalData.MandatoryPrefixes := PP2MndPrf[P and $03];
  if not PInst^.Next() then
    Exit;
  UpdateOpSizeW(PInst);
  TABLE_2[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_LOCK_void(PInst: PInstruction);
begin
  { F0 }
  if PInst^.Prefixes.LockPrf.Count = 0 then
  begin
    PInst^.InternalData.MandatoryPrefixes := PInst^.InternalData.MandatoryPrefixes or MND_PRF_F0;
    PInst^.Prefixes.LockPrf.Flags := PF_USED;
  end
  else
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.LockPrf);
    // PInst^.Warn(WARN_SUPERFLUOUS_LOCK_PREFIX);
  end;
  Inc(PInst^.Prefixes.LockPrf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

procedure Decode_PREFIXES_BND_void(PInst: PInstruction);
begin
  { F2 }
end;

procedure Decode_PREFIXES_REPNE_void(PInst: PInstruction);
begin
  { F2 }
end;

procedure Decode_PREFIXES_XAQUIRE_void(PInst: PInstruction);
begin
  { F2 }
end;

procedure Decode_PREFIXES_F2_void(PInst: PInstruction);
begin
  { F2 }
  ClearPrfNA(PInst);
  if PInst^.Prefixes.F2Prf.Count = 0 then
  begin
    PInst^.InternalData.MandatoryPrefixes := PInst^.InternalData.MandatoryPrefixes or MND_PRF_F2;
    PInst^.Prefixes.F2Prf.Flags := PF_USED;
  end
  else
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.F2Prf);
    // PInst^.Warn(WARN_SUPERFLUOUS_F2_PREFIX);
  end;
  Inc(PInst^.Prefixes.F2Prf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);

end;

procedure Decode_PREFIXES_REP_void(PInst: PInstruction);
begin
  { F3 }
end;

procedure Decode_PREFIXES_REPE_void(PInst: PInstruction);
begin
  { F3 }
end;

procedure Decode_PREFIXES_XRELEASE_void(PInst: PInstruction);
begin
  { F3 }
end;

procedure Decode_PREFIXES_F3_void(PInst: PInstruction);
begin
  { F3 }
  ClearPrfNA(PInst);
  if PInst^.Prefixes.F3Prf.Count = 0 then
  begin
    PInst^.InternalData.MandatoryPrefixes := PInst^.InternalData.MandatoryPrefixes or MND_PRF_F3;
    PInst^.Prefixes.F3Prf.Flags := PF_USED;
  end
  else
  begin
    SuperfluousPrefix(PInst, PInst^.Prefixes.F3Prf);
    // PInst^.Warn(WARN_SUPERFLUOUS_F3_PREFIX);
  end;
  Inc(PInst^.Prefixes.F3Prf.Count);
  Inc(PInst^.Prefixes.Count);
  TABLE_1[PInst^.NextInst^](PInst);
end;

end.
