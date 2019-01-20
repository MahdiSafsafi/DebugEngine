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
// The Original Code is SSE5A.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_SSE5A_PROTB_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PROTB  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTB;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTW_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PROTW  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTW;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTD_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PROTD  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTD;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTQ_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PROTQ  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTQ;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_COMccPS_VDo_Vo_Wo(PInst: PInstruction);
begin
  // COMccPS  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDo_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_COMccPS_Array[PInst^.OpImmExt and $0F];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_COMccPS_Array[PInst^.OpImmExt and $0F]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_COMccPD_VDo_Vo_Wo(PInst: PInstruction);
begin
  // COMccPD  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDo_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_COMccPD_Array[PInst^.OpImmExt and $0F];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_COMccPD_Array[PInst^.OpImmExt and $0F]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_COMccSS_VDd_Vd_Wd(PInst: PInstruction);
begin
  // COMccSS  VDd,Vd,Wd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDd_Vd_Wd(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_COMccSS_Array[PInst^.OpImmExt and $0F];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_COMccSS_Array[PInst^.OpImmExt and $0F]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_COMccSD_VDq_Vq_Wq(PInst: PInstruction);
begin
  // COMccSD  VDq,Vq,Wq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDq_Vq_Wq(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_COMccSD_Array[PInst^.OpImmExt and $0F];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_COMccSD_Array[PInst^.OpImmExt and $0F]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCOMccB_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PCOMccB  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDo_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_PCOMccB_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_PCOMccB_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCOMccW_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PCOMccW  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDo_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_PCOMccW_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_PCOMccW_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCOMccD_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PCOMccD  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDo_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_PCOMccD_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_PCOMccD_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCOMccQ_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PCOMccQ  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDo_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_PCOMccQ_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_PCOMccQ_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCOMccUB_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PCOMccUB  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDo_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_PCOMccUB_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_PCOMccUB_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCOMccUW_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PCOMccUW  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDo_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_PCOMccUW_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_PCOMccUW_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCOMccUD_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PCOMccUD  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDo_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_PCOMccUD_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_PCOMccUD_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCOMccUQ_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PCOMccUQ  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  Decode_VDo_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_PCOMccUQ_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_PCOMccUQ_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FRCZPS_Vo_Wo(PInst: PInstruction);
begin
  // FRCZPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FRCZPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FRCZPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FRCZPD_Vo_Wo(PInst: PInstruction);
begin
  // FRCZPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FRCZPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FRCZPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FRCZSS_Vd_Wd(PInst: PInstruction);
begin
  // FRCZSS  Vd,Wd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FRCZSS;
  Decode_Vd_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FRCZSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FRCZSD_Vq_Wq(PInst: PInstruction);
begin
  // FRCZSD  Vq,Wq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FRCZSD;
  Decode_Vq_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FRCZSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_CVTPH2PS_Vo_Wq(PInst: PInstruction);
begin
  // CVTPH2PS  Vo,Wq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CVTPH2PS;
  Decode_Vo_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPH2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_CVTPS2PH_Wq_Vo(PInst: PInstruction);
begin
  // CVTPS2PH  Wq,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CVTPS2PH;
  Decode_Wq_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPS2PH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDBW_Vo_Wo(PInst: PInstruction);
begin
  // PHADDBW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDBW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDBD_Vo_Wo(PInst: PInstruction);
begin
  // PHADDBD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDBD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDBQ_Vo_Wo(PInst: PInstruction);
begin
  // PHADDBQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDBQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDWD_Vo_Wo(PInst: PInstruction);
begin
  // PHADDWD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDWD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDWQ_Vo_Wo(PInst: PInstruction);
begin
  // PHADDWQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDWQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDWQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDDQ_Vo_Wo(PInst: PInstruction);
begin
  // PHADDDQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDDQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDUBW_Vo_Wo(PInst: PInstruction);
begin
  // PHADDUBW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDUBW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDUBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDUBD_Vo_Wo(PInst: PInstruction);
begin
  // PHADDUBD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDUBD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDUBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDUBQ_Vo_Wo(PInst: PInstruction);
begin
  // PHADDUBQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDUBQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDUBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDUWD_Vo_Wo(PInst: PInstruction);
begin
  // PHADDUWD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDUWD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDUWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDUWQ_Vo_Wo(PInst: PInstruction);
begin
  // PHADDUWQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDUWQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDUWQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHADDUDQ_Vo_Wo(PInst: PInstruction);
begin
  // PHADDUDQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDUDQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDUDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHSUBBW_Vo_Wo(PInst: PInstruction);
begin
  // PHSUBBW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHSUBBW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHSUBBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHSUBWD_Vo_Wo(PInst: PInstruction);
begin
  // PHSUBWD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHSUBWD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHSUBWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PHSUBDQ_Vo_Wo(PInst: PInstruction);
begin
  // PHSUBDQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHSUBDQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHSUBDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDPS_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // FMADDPS  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDPS;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDPS_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // FMADDPS  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDPS;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDPD_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // FMADDPD  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDPD;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDPD_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // FMADDPD  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDPD;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDSS_VDd_VDd_Vd_Wd(PInst: PInstruction);
begin
  // FMADDSS  VDd,VDd,Vd,Wd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDSS;
  Decode_VDd_VDd_Vd_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDSS_VDd_VDd_Wd_Vd(PInst: PInstruction);
begin
  // FMADDSS  VDd,VDd,Wd,Vd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDSS;
  Decode_VDd_VDd_Wd_Vd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDSD_VDq_VDq_Vq_Wq(PInst: PInstruction);
begin
  // FMADDSD  VDq,VDq,Vq,Wq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDSD;
  Decode_VDq_VDq_Vq_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDSD_VDq_VDq_Wq_Vq(PInst: PInstruction);
begin
  // FMADDSD  VDq,VDq,Wq,Vq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDSD;
  Decode_VDq_VDq_Wq_Vq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDPS_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // FMADDPS  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDPS;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDPS_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // FMADDPS  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDPS;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDPD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // FMADDPD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDPD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDPD_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // FMADDPD  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDPD;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDSS_VDd_Vd_Wd_VDd(PInst: PInstruction);
begin
  // FMADDSS  VDd,Vd,Wd,VDd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDSS;
  Decode_VDd_Vd_Wd_VDd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDSS_VDd_Wd_Vd_VDd(PInst: PInstruction);
begin
  // FMADDSS  VDd,Wd,Vd,VDd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDSS;
  Decode_VDd_Wd_Vd_VDd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDSD_VDq_Vq_Wq_VDq(PInst: PInstruction);
begin
  // FMADDSD  VDq,Vq,Wq,VDq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDSD;
  Decode_VDq_Vq_Wq_VDq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMADDSD_VDq_Wq_Vq_VDq(PInst: PInstruction);
begin
  // FMADDSD  VDq,Wq,Vq,VDq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMADDSD;
  Decode_VDq_Wq_Vq_VDq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBPS_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // FMSUBPS  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBPS;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBPS_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // FMSUBPS  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBPS;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBPD_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // FMSUBPD  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBPD;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBPD_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // FMSUBPD  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBPD;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBSS_VDd_VDd_Vd_Wd(PInst: PInstruction);
begin
  // FMSUBSS  VDd,VDd,Vd,Wd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBSS;
  Decode_VDd_VDd_Vd_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBSS_VDd_VDd_Wd_Vd(PInst: PInstruction);
begin
  // FMSUBSS  VDd,VDd,Wd,Vd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBSS;
  Decode_VDd_VDd_Wd_Vd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBSD_VDq_VDq_Vq_Wq(PInst: PInstruction);
begin
  // FMSUBSD  VDq,VDq,Vq,Wq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBSD;
  Decode_VDq_VDq_Vq_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBSD_VDq_VDq_Wq_Vq(PInst: PInstruction);
begin
  // FMSUBSD  VDq,VDq,Wq,Vq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBSD;
  Decode_VDq_VDq_Wq_Vq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBPS_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // FMSUBPS  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBPS;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBPS_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // FMSUBPS  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBPS;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBPD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // FMSUBPD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBPD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBPD_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // FMSUBPD  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBPD;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBSS_VDd_Vd_Wd_VDd(PInst: PInstruction);
begin
  // FMSUBSS  VDd,Vd,Wd,VDd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBSS;
  Decode_VDd_Vd_Wd_VDd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBSS_VDd_Wd_Vd_VDd(PInst: PInstruction);
begin
  // FMSUBSS  VDd,Wd,Vd,VDd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBSS;
  Decode_VDd_Wd_Vd_VDd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBSD_VDq_Vq_Wq_VDq(PInst: PInstruction);
begin
  // FMSUBSD  VDq,Vq,Wq,VDq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBSD;
  Decode_VDq_Vq_Wq_VDq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FMSUBSD_VDq_Wq_Vq_VDq(PInst: PInstruction);
begin
  // FMSUBSD  VDq,Wq,Vq,VDq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMSUBSD;
  Decode_VDq_Wq_Vq_VDq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDPS_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // FNMADDPS  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDPS;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDPS_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // FNMADDPS  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDPS;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDPD_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // FNMADDPD  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDPD;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDPD_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // FNMADDPD  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDPD;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDSS_VDd_VDd_Vd_Wd(PInst: PInstruction);
begin
  // FNMADDSS  VDd,VDd,Vd,Wd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDSS;
  Decode_VDd_VDd_Vd_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDSS_VDd_VDd_Wd_Vd(PInst: PInstruction);
begin
  // FNMADDSS  VDd,VDd,Wd,Vd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDSS;
  Decode_VDd_VDd_Wd_Vd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDSD_VDq_VDq_Vq_Wq(PInst: PInstruction);
begin
  // FNMADDSD  VDq,VDq,Vq,Wq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDSD;
  Decode_VDq_VDq_Vq_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDSD_VDq_VDq_Wq_Vq(PInst: PInstruction);
begin
  // FNMADDSD  VDq,VDq,Wq,Vq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDSD;
  Decode_VDq_VDq_Wq_Vq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDPS_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // FNMADDPS  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDPS;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDPS_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // FNMADDPS  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDPS;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDPD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // FNMADDPD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDPD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDPD_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // FNMADDPD  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDPD;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDSS_VDd_Vd_Wd_VDd(PInst: PInstruction);
begin
  // FNMADDSS  VDd,Vd,Wd,VDd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDSS;
  Decode_VDd_Vd_Wd_VDd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDSS_VDd_Wd_Vd_VDd(PInst: PInstruction);
begin
  // FNMADDSS  VDd,Wd,Vd,VDd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDSS;
  Decode_VDd_Wd_Vd_VDd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDSD_VDq_Vq_Wq_VDq(PInst: PInstruction);
begin
  // FNMADDSD  VDq,Vq,Wq,VDq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDSD;
  Decode_VDq_Vq_Wq_VDq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMADDSD_VDq_Wq_Vq_VDq(PInst: PInstruction);
begin
  // FNMADDSD  VDq,Wq,Vq,VDq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMADDSD;
  Decode_VDq_Wq_Vq_VDq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBPS_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // FNMSUBPS  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBPS;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBPS_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // FNMSUBPS  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBPS;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBPD_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // FNMSUBPD  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBPD;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBPD_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // FNMSUBPD  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBPD;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBSS_VDd_VDd_Vd_Wd(PInst: PInstruction);
begin
  // FNMSUBSS  VDd,VDd,Vd,Wd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBSS;
  Decode_VDd_VDd_Vd_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBSS_VDd_VDd_Wd_Vd(PInst: PInstruction);
begin
  // FNMSUBSS  VDd,VDd,Wd,Vd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBSS;
  Decode_VDd_VDd_Wd_Vd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBSD_VDq_VDq_Vq_Wq(PInst: PInstruction);
begin
  // FNMSUBSD  VDq,VDq,Vq,Wq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBSD;
  Decode_VDq_VDq_Vq_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBSD_VDq_VDq_Wq_Vq(PInst: PInstruction);
begin
  // FNMSUBSD  VDq,VDq,Wq,Vq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBSD;
  Decode_VDq_VDq_Wq_Vq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBPS_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // FNMSUBPS  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBPS;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBPS_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // FNMSUBPS  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBPS;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBPD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // FNMSUBPD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBPD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBPD_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // FNMSUBPD  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBPD;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBSS_VDd_Vd_Wd_VDd(PInst: PInstruction);
begin
  // FNMSUBSS  VDd,Vd,Wd,VDd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBSS;
  Decode_VDd_Vd_Wd_VDd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBSS_VDd_Wd_Vd_VDd(PInst: PInstruction);
begin
  // FNMSUBSS  VDd,Wd,Vd,VDd
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBSS;
  Decode_VDd_Wd_Vd_VDd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBSD_VDq_Vq_Wq_VDq(PInst: PInstruction);
begin
  // FNMSUBSD  VDq,Vq,Wq,VDq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBSD;
  Decode_VDq_Vq_Wq_VDq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_FNMSUBSD_VDq_Wq_Vq_VDq(PInst: PInstruction);
begin
  // FNMSUBSD  VDq,Wq,Vq,VDq
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNMSUBSD;
  Decode_VDq_Wq_Vq_VDq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNMSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PERMPS_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PERMPS  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PERMPS;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PERMPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PERMPS_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PERMPS  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PERMPS;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PERMPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PERMPD_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PERMPD  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PERMPD;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PERMPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PERMPD_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PERMPD  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PERMPD;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PERMPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCMOV_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PCMOV  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PCMOV;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCMOV_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PCMOV  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PCMOV;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PPERM_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PPERM  VDo,VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PPERM;
  Decode_VDo_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PPERM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PPERM_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PPERM  VDo,VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PPERM;
  Decode_VDo_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PPERM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PERMPS_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PERMPS  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PERMPS;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PERMPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PERMPS_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // PERMPS  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PERMPS;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PERMPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PERMPD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PERMPD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PERMPD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PERMPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCMOV_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PCMOV  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PCMOV;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PCMOV_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // PCMOV  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PCMOV;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PPERM_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PPERM  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PPERM;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PPERM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PPERM_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  // PPERM  VDo,Wo,Vo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PPERM;
  Decode_VDo_Wo_Vo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PPERM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTB_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PROTB  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTB;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTB_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PROTB  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTB;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTW_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PROTW  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTW;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTW_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PROTW  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTW;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTD_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PROTD  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTD;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTD_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PROTD  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTD;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTQ_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PROTQ  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTQ;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PROTQ_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PROTQ  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PROTQ;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PROTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHLB_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PSHLB  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHLB;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHLB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHLB_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PSHLB  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHLB;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHLB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHLW_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PSHLW  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHLW;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHLW_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PSHLW  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHLW;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHLD_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PSHLD  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHLD;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHLD_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PSHLD  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHLD;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHLQ_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PSHLQ  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHLQ;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHLQ_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PSHLQ  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHLQ;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHAB_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PSHAB  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHAB;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHAB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHAB_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PSHAB  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHAB;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHAB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHAW_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PSHAW  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHAW;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHAW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHAW_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PSHAW  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHAW;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHAW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHAD_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PSHAD  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHAD;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHAD_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PSHAD  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHAD;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHAQ_VDo_Vo_Wo(PInst: PInstruction);
begin
  // PSHAQ  VDo,Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHAQ;
  Decode_VDo_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHAQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PSHAQ_VDo_Wo_Vo(PInst: PInstruction);
begin
  // PSHAQ  VDo,Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHAQ;
  Decode_VDo_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHAQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMACSSWW_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMACSSWW  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMACSSWW;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMACSSWW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMACSSWD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMACSSWD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMACSSWD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMACSSWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMACSSDQL_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMACSSDQL  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMACSSDQL;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMACSSDQL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMACSSDD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMACSSDD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMACSSDD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMACSSDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMACSSDQH_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMACSSDQH  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMACSSDQH;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMACSSDQH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMACSWW_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMACSWW  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMACSWW;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMACSWW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMACSWD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMACSWD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMACSWD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMACSWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMACSDQL_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMACSDQL  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMACSDQL;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMACSDQL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMACSDD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMACSDD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMACSDD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMACSDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMACSDQH_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMACSDQH  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMACSDQH;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMACSDQH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMADCSSWD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMADCSSWD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMADCSSWD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMADCSSWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE5A_PMADCSWD_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  // PMADCSWD  VDo,Vo,Wo,VDo
  PInst^.InstGroups := INST_GRP_SSE5A;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMADCSWD;
  Decode_VDo_Vo_Wo_VDo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMADCSWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
