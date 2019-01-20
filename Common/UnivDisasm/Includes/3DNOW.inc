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
// The Original Code is 3DNOW.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_3DNOW_PI2FW_Pq_Qq(PInst: PInstruction);
begin
  // PI2FW  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PI2FW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PI2FW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PI2FD_Pq_Qq(PInst: PInstruction);
begin
  // PI2FD  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PI2FD;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PI2FD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PF2IW_Pq_Qq(PInst: PInstruction);
begin
  // PF2IW  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PF2IW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PF2IW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PF2ID_Pq_Qq(PInst: PInstruction);
begin
  // PF2ID  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PF2ID;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PF2ID);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFRCPV_Pq_Qq(PInst: PInstruction);
begin
  // PFRCPV  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFRCPV;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFRCPV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFRSQRTV_Pq_Qq(PInst: PInstruction);
begin
  // PFRSQRTV  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFRSQRTV;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFRSQRTV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFNACC_Pq_Qq(PInst: PInstruction);
begin
  // PFNACC  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFNACC;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFNACC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFPNACC_Pq_Qq(PInst: PInstruction);
begin
  // PFPNACC  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFPNACC;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFPNACC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFCMPGE_Pq_Qq(PInst: PInstruction);
begin
  // PFCMPGE  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFCMPGE;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFCMPGE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFMIN_Pq_Qq(PInst: PInstruction);
begin
  // PFMIN  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFMIN;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFMIN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFRCP_Pq_Qq(PInst: PInstruction);
begin
  // PFRCP  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFRCP;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFRCP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFRSQRT_Pq_Qq(PInst: PInstruction);
begin
  // PFRSQRT  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFRSQRT;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFRSQRT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFSUB_Pq_Qq(PInst: PInstruction);
begin
  // PFSUB  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFSUB;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFADD_Pq_Qq(PInst: PInstruction);
begin
  // PFADD  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFADD;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFCMPGT_Pq_Qq(PInst: PInstruction);
begin
  // PFCMPGT  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFCMPGT;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFCMPGT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFMAX_Pq_Qq(PInst: PInstruction);
begin
  // PFMAX  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFMAX;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFMAX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFRCPIT1_Pq_Qq(PInst: PInstruction);
begin
  // PFRCPIT1  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFRCPIT1;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFRCPIT1);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFRSQIT1_Pq_Qq(PInst: PInstruction);
begin
  // PFRSQIT1  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFRSQIT1;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFRSQIT1);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFSUBR_Pq_Qq(PInst: PInstruction);
begin
  // PFSUBR  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFSUBR;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFACC_Pq_Qq(PInst: PInstruction);
begin
  // PFACC  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFACC;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFACC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFCMPEQ_Pq_Qq(PInst: PInstruction);
begin
  // PFCMPEQ  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFCMPEQ;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFCMPEQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFMUL_Pq_Qq(PInst: PInstruction);
begin
  // PFMUL  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFMUL;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PFRCPIT2_Pq_Qq(PInst: PInstruction);
begin
  // PFRCPIT2  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PFRCPIT2;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PFRCPIT2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PMULHRW_Pq_Qq(PInst: PInstruction);
begin
  // PMULHRW  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMULHRW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULHRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PSWAPD_Pq_Qq(PInst: PInstruction);
begin
  // PSWAPD  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSWAPD;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSWAPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_3DNOW_PAVGUSB_Pq_Qq(PInst: PInstruction);
begin
  // PAVGUSB  Pq,Qq
  PInst^.InstGroups := INST_GRP_3DNOW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PAVGUSB;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PAVGUSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
