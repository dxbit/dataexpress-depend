{$IFDEF DELPHI}
  {$DEFINE PS_RESBEFOREPARAMETERS}
  {$DEFINE x64_string_result_as_varparameter}
{$ENDIF}

{ implementation of x64 abi }
//procedure DebugBreak; external 'Kernel32.dll';
const
  EmptyPchar: array[0..0] of char = #0;
{$IFDEF FPC}
{$ASMMODE INTEL}
{$ENDIF}
{$IFDEF MSWINDOWS}{$DEFINE WINDOWS}{$ENDIF}

{$IFDEF WINDOWS}
type
  TRegisters = packed record
    _RCX,                  // 0
    _RDX,                  // 8
    _R8,                   // 16
    _R9: IPointer;         // 24
    _XMM1,                 // 32
    _XMM2,                 // 40
    _XMM3: Double;         // 48
    Stack: Pointer;        // 56
    Items: {$IFDEF FPC}PtrUInt{$ELSE}IntPtr{$ENDIF}; // 64
    SingleBits: Integer; // 72
  end;

procedure x64call_impl(
  Address: Pointer;
  out _RAX: IPointer;
  var _XMM0: Double;
  var Registers: TRegisters); assembler;
var
  CallFrame: array[0..259] of IPointer;
asm
(* Registers:
    RCX: Address
    RDX: *_RAX
    R8:  * _XMM0
    R9: _REGISTERS
    fpc inserts an 20h empty space
*)
  mov [CallFrame+2048], rcx
  mov [CallFrame+2056], rdx
  mov [CallFrame+2064], r8
  mov [CallFrame+2072], r9

  mov rax, r9

  mov rcx, [rax+64]
  mov rdx, [rax+56]
  lea r10, [rsp+32]
  lea r10, [r10+rcx*8-8]
  jmp @compareitems
@work:
  mov r11, [rdx]
  mov [r10], r11
  dec rcx
  sub rdx,8
  sub r10,8
@compareitems:
  or rcx, rcx
  jnz @work

  // copy registers
  mov rcx, [rax+72] // single bits

  bt rcx, 1
  jnc @g1
  cvtsd2ss xmm1, [rax+32]
  jmp @g1e
  @g1:
  movsd xmm1, [rax+32]
  @g1e:


  bt rcx, 2
  jnc @g2
  cvtsd2ss xmm2, [rax+40]
  jmp @g2e
  @g2:
  movsd xmm2, [rax+40]
  @g2e:

  bt rcx, 3
  jnc @g3
  cvtsd2ss xmm3, [rax+48]
  jmp @g3e
  @g3:
  movsd xmm3, [rax+48]
  @g3e:



  bt rcx, 0
  jnc @g0
  mov rdx, [CallFrame+2064]
  cvtsd2ss xmm0, [rdx]
  jmp @g0e
  @g0:
  mov rdx, [CallFrame+2064]
  movsd xmm0, [rdx]
  @g0e:

  // other registers
  mov rcx, [rax]
  mov rdx, [rax+8]
  mov r8, [rax+16]
  mov r9, [rax+24]


  mov RAX, [CallFrame+2048]

  call RAX

  // copy result back
  mov RDX, [CallFrame+2056]
  mov [RDX], RAX

  mov rax, [CallFrame+2072]

  bt [rax+72], 8                 // if atype.basetype  <> btSingle
  jnc @g5                        //
  cvtss2sd xmm1,xmm0             // convert single to double  into xmm1
  mov rdx,[CallFrame+2064]
  movsd qword ptr [rdx], xmm1    // save  xmm1 to param _xmm0
  jmp @g5e                       // exit if atype.basetype  = btSingle

  @g5:                           //else "if atype.basetype  = btSingle"
    mov rdx,[CallFrame+2064]
    movsd qword ptr [rdx], xmm0  // save  xmm1 to param _xmm0

  @g5e:
end;

procedure x64call(
  Address: Pointer;
  out _RAX: IPointer;
  var _XMM0: Double;
  var Registers: TRegisters);
begin
  x64call_impl(Address, _RAX, _XMM0, Registers);
end;

{$ELSE}
type
  TRegisters = packed record
    _RDI,               //  0
    _RSI,               //  8
    _RDX,               // 16
    _RCX,               // 24
    _R8,                // 32
    _R9: IPointer;      // 40
    _XMM1,              // 48
    _XMM2,              // 56
    _XMM3,              // 64
    _XMM4,              // 72
    _XMM5,              // 80
    _XMM6,              // 88
    _XMM7: Double;      // 96
    SingleBits: Integer; //104
  end;

procedure x64call(
  Address: Pointer;
  out _RAX: IPointer;

  var Registers: TRegisters;
  aStack: Pointer; aItems: PtrUInt; var _XMM0: Double;
  out _RDX: IPointer); assembler; nostackframe;


asm
(* Registers:
    RDI: Address
    RSI: _RAX
    RDX: Registers
    RCX: aStack
    R8:  aItems
    R9:  XMM0

    rbp-8    addr
    rbp-16   _rax
    rbp-24   _xmm0
    rbp-32   regs
    rbp-40   _rdx (7th param, taken from the caller stack)
*)
  push rbp
  mov rbp,rsp
  push rdi  // address
  push rsi  // _rax
  push r9   // xmm0
  push rdx  // regs
  mov rax, [rsp+48]
  push rax  // _rdx
  // rsp = rbp-40
  mov rax, r8
  shl rax, 3
  sub rsp, rax          // reserve stack-argument area (aItems*8)
  and rsp, -16          // SysV: rsp must be 16-byte aligned at the call
  // copy the PStack array into the reserved area, byte order preserved:
  //   first stack argument (lowest address) = PStack[0..7]
  mov rdi, rax          // aItems*8
  lea rdx, [rsp+rdi]    // rdx = top of reserved area
  test rax, rax
  jz @skipcopy          // items=0: no stack arguments to copy (aStack=nil)
@copyback:
  mov rdi, [rcx]        // load PStack[slot]
  sub rdx, 8
  mov [rdx], rdi        // write into reserved area
  sub rcx, 8
  dec r8
  jnz @copyback
@skipcopy:

  // copy registers
  mov rax, [rbp-32]     // Registers
  // xmm0
  mov rdx,[rbp-24]
  bt [rax+104], 0
  jnc @skipxmm0
  cvtsd2ss xmm0,[rdx]
  jmp @skipxmm0re
  @skipxmm0:
  movq xmm0,[rdx]            // move quadword to xmm0 from _XMM0
  @skipxmm0re:

  // xmm1
  bt [rax+104], 1
  jnc @skipxmm1
  cvtsd2ss xmm1,[rax+48]
  jmp @skipxmm1re
  @skipxmm1:
  movq xmm1,[rax+48]         // move quadword to xmm1 from Registers._XMM1
  @skipxmm1re:

  // xmm2
  bt [rax+104], 2
  jnc @skipxmm2
  cvtsd2ss xmm2,[rax+56]
  jmp @skipxmm2re
  @skipxmm2:
  movq xmm2,[rax+56]         // move quadword to xmm2 from Registers._XMM2
  @skipxmm2re:

  // xmm3
  bt [rax+104], 3
  jnc @skipxmm3
  cvtsd2ss xmm3,[rax+64]
  jmp @skipxmm3re
  @skipxmm3:
  movq xmm3,[rax+64]         // move quadword to xmm3 from Registers._XMM3
  @skipxmm3re:

  // xmm4
  bt [rax+104], 4
  jnc @skipxmm4
  cvtsd2ss xmm4,[rax+72]
  jmp @skipxmm4re
  @skipxmm4:
  movq xmm4,[rax+72]         // move quadword to xmm4 from Registers._XMM4
  @skipxmm4re:

  // xmm5
  bt [rax+104], 5
  jnc @skipxmm5
  cvtsd2ss xmm5,[rax+80]
  jmp @skipxmm5re
  @skipxmm5:
  movq xmm5,[rax+80]         // move quadword to xmm5 from Registers._XMM5
  @skipxmm5re:

  // xmm6
  bt [rax+104], 6
  jnc @skipxmm6
  cvtsd2ss xmm6,[rax+88]
  jmp @skipxmm6re
  @skipxmm6:
  movq xmm6,[rax+88]         // move quadword to xmm6 from Registers._XMM6
  @skipxmm6re:

// xmm7
  bt [rax+104], 7
  jnc @skipxmm7
  cvtsd2ss xmm7,[rax+96]
  jmp @skipxmm7re
  @skipxmm7:
  movq xmm7,[rax+96]         // move quadword to xmm7 from Registers._XMM7
  @skipxmm7re:


  mov RDI, [rax]
  mov RSI, [rax+ 8]
  mov RDX, [rax+16]
  mov RCX, [rax+24]
  mov R8,  [rax+32]
  mov R9,  [rax+40]

  // weird thing on windows, it needs 32 bytes in the CALLEE side to do whatever in; not sure about linux
  //sub RSP, 32

  mov rax, [rbp-8]
  call RAX

//  add rsp, 8

  // add RSP, 32 // undo the damage done earlier

  // copy result back
  mov rsi, [rbp-16]          // _RAX parameter
  mov [rsi], RAX
  mov rsi, [rbp-40]          // _RDX parameter
  mov [rsi], RDX             // (RDX holds the call result; 16-byte record results)
  mov rsi, [rbp-24]          // _XMM0 parameter

  // xmm0 res
  mov rax, [rbp-32]          // Registers parameter
  bt [rax+104], 8            // if atype.basetype  <> btSingle
  jnc @skipres               // then goto skipres else begin
  cvtss2sd xmm1,xmm0         // convert single to double  into xmm1
  movq [rsi],xmm1            // move quadword to _XMM0
  jmp @skipresre             // end
  @skipres:
  movq [rsi],xmm0            // move quadword to _XMM0
  @skipresre:


  pop rax  // _rdx slot
  pop rdx  // regs
  pop r9   // xmm0
  pop rsi  // _rax
  pop rdi  // address
  leave
  ret
end;
{$ENDIF}

function TPSExec.InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention; Params: TPSList; res: PPSVariantIFC): Boolean;
var
  Stack: array of Byte;
  _RAX: IPointer;
  _RDX: IPointer;
_XMM0: Double;
  Registers: TRegisters;
{$IFNDEF WINDOWS}
  RegUsageFloat: Byte;
{$ENDIF}
  RegUsage: Byte;
  CallData: TPSList;
  I: Integer;
  pp: ^Byte;
  // 7bit
  {$IFDEF FPC}
  IsVirtualCons: Boolean;
  tmp: Extended;
  {$ENDIF}
  //

  function rp(p: PPSVariantIFC): PPSVariantIFC;
  begin
    if p = nil then
    begin
      result := nil;
      exit;
    end;
    if p.aType.BaseType = btPointer then
    begin
      p^.aType := Pointer(Pointer(IPointer(p^.dta) + PointerSize)^);
      p^.Dta := Pointer(p^.dta^);
    end;
    Result := p;
  end;
{$IFDEF WINDOWS}
  procedure StoreReg(data: IPointer);   overload;
  var p: Pointer;
  begin
    case RegUsage of
      0: begin inc(RegUsage); Registers._RCX:=Data; end;
      1: begin inc(RegUsage); Registers._RDX:=Data; end;
      2: begin inc(RegUsage); Registers._R8:=Data; end;
      3: begin inc(RegUsage); Registers._R9:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      IPointer(p^) := data;
    end;
    end;
  end;
  {$ELSE}
  procedure StoreReg(data: IPointer);   overload;
  var p: Pointer;
  begin
    case RegUsage of
      0: begin inc(RegUsage); Registers._RDI:=Data; end;
      1: begin inc(RegUsage); Registers._RSI:=Data; end;
      2: begin inc(RegUsage); Registers._RDX:=Data; end;
      3: begin inc(RegUsage); Registers._RCX:=Data; end;
      4: begin inc(RegUsage); Registers._R8:=Data; end;
      5: begin inc(RegUsage); Registers._R9:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      IPointer(p^) := data;
    end;
    end;
  end;
{$ENDIF}

  procedure StoreStack(const aData; Len: Integer);
  var
    p: Pointer;
  begin
    SetLength(Stack, Length(Stack)+Len);
    p := @Stack[Length(Stack)-Len];
    Move(aData, p^, Len);
    // 7bit. Переместил этот код с начала в конец.
    if Len > 8 then
      if Length(Stack) mod 16 <> 0 then begin
        SetLength(Stack, Length(Stack)+ (16-(Length(Stack) mod 16)));
      end;
    //
  end;

{$IFDEF WINDOWS}
  procedure StoreReg(data: Double); overload;
  var p: Pointer;
  begin
    case RegUsage of
      0: begin inc(RegUsage); _XMM0:=Data; end;
      1: begin inc(RegUsage); Registers._XMM1:=Data; end;
      2: begin inc(RegUsage); Registers._XMM2:=Data; end;
      3: begin inc(RegUsage); Registers._XMM3:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      Double(p^) := data;
    end;
    end;
  end;
  procedure StoreReg(data: Single); overload;
  var p: Pointer;
  begin
    case RegUsage of
      0: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 1;_XMM0:=Data; end;
      1: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 2; Registers._XMM1:=Data; end;
      2: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 4;Registers._XMM2:=Data; end;
      3: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 8; Registers._XMM3:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      Double(p^) := data;
    end;
    end;
  end;
  {$ELSE}
  procedure StoreReg(data: Double); overload;
  var p: Pointer;
  begin
    case RegUsageFloat of
      0: begin inc(RegUsageFloat); _XMM0:=Data; end;
      1: begin inc(RegUsageFloat); Registers._XMM1:=Data; end;
      2: begin inc(RegUsageFloat); Registers._XMM2:=Data; end;
      3: begin inc(RegUsageFloat); Registers._XMM3:=Data; end;
      4: begin inc(RegUsageFloat); Registers._XMM4:=Data; end;
      5: begin inc(RegUsageFloat); Registers._XMM5:=Data; end;
      6: begin inc(RegUsageFloat); Registers._XMM6:=Data; end;
      7: begin inc(RegUsageFloat); Registers._XMM7:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      Double(p^) := data;
    end;
    end;
  end;
  procedure StoreReg(data: Single); overload;
  var p: Pointer;
  begin
    case RegUsageFloat of
      0: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 1; _XMM0:=Data; end;
      1: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 2; Registers._XMM1:=Data; end;
      2: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 4; Registers._XMM2:=Data; end;
      3: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 8; Registers._XMM3:=Data; end;
      4: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 16; Registers._XMM4:=Data; end;
      5: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 32; Registers._XMM5:=Data; end;
      6: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 64; Registers._XMM6:=Data; end;
      7: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 128; Registers._XMM7:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      Double(p^) := data;
    end;
    end;
  end;
  {$ENDIF}
  function GetPtr(fVar: PPSVariantIFC): Boolean;
  var
    varPtr: Pointer;
    //UseReg: Boolean;
    //tempstr: tbtstring;
    p: Pointer;
  begin
    Result := False;
    if FVar = nil then exit;
    if fVar.VarParam then
    begin
      case fvar.aType.BaseType of
        btArray:
          begin
            if Copy(fvar.aType.ExportName, 1, 10) = '!OPENARRAY' then
            begin
              p := CreateOpenArray(True, Self, FVar);
              if p = nil then exit;
              CallData.Add(p);
              StoreReg(IPointer(POpenArray(p)^.Data));
              StoreReg(IPointer(POpenArray(p)^.ItemCount -1));
              Result := True;
              Exit;
            end else begin
              varptr := fvar.Dta;
//              Exit;
            end;
          end;
        btVariant,
        btSet,
        btStaticArray,
        btRecord,
        btInterface,
        btClass,
        {$IFNDEF PS_NOWIDESTRING} btUnicodeString, btWideString, btWideChar, {$ENDIF} btU8, btS8, btU16,
        btS16, btU32, btS32, btSingle, btDouble, btExtended, btString, btPChar, btChar, btCurrency
        {$IFNDEF PS_NOINT64}, bts64{$ENDIF}:
          begin
            Varptr := fvar.Dta;
          end;
      else begin
          exit; //invalid type
        end;
      end; {case}

      StoreReg(IPointer(VarPtr));
    end else begin
//      UseReg := True;
      case fVar^.aType.BaseType of
        btSet:
          begin
            case TPSTypeRec_Set(fvar.aType).aByteSize of
              1: StoreReg(IPointer(byte(fvar.dta^)));
              2: StoreReg(IPointer(word(fvar.dta^)));
              3, 4: StoreReg(IPointer(cardinal(fvar.dta^)));
              5,6,7,8: StoreReg(IPointer(fVar.Dta^));
              else
                StoreReg(IPointer(fvar.Dta));
            end;
          end;
        btArray:
          begin
            if Copy(fvar^.aType.ExportName, 1, 10) = '!OPENARRAY' then
            begin
              p := CreateOpenArray(False, SElf, FVar);
              if p =nil then exit;
              CallData.Add(p);
              StoreReg(IPointer(POpenArray(p)^.Data));
              StoreReg(IPointer(POpenArray(p)^.ItemCount -1));
              Result := True;
              exit;
            end else begin
            {$IFDEF FPC}
              StoreReg(IPointer(FVar.Dta));
            {$ELSE}
              StoreReg(IPointer(FVar.Dta^));
            {$ENDIF}
            end;
          end;
        btRecord:
          begin
            if fvar^.aType.RealSize <= sizeof(IPointer) then
              StoreReg(IPointer(fvar.dta^))
            {$IFDEF WINDOWS}
            else
              StoreReg(IPointer(fvar.Dta));
            {$ELSE}
            // 7bit 2 регистра
            else if fvar^.aType.RealSize = 16 then
            begin
              StoreReg(IPointer(fvar.dta^));
              StoreReg(IPointer((fvar.dta+8)^));
            end
            //
            else
{$IFDEF FPC}
            begin
              StoreStack(fVar.Dta^, fVar^.aType.RealSize + ((PointerSize - (fVar^.aType.RealSize mod PointerSize)) mod PointerSize));
            end
{$ELSE}
              StoreStack(fVar.Dta^, fVar^.aType.RealSize);
{$ENDIF}
            {$ENDIF}
          end;
        btVariant
        , btStaticArray:
          begin
            StoreReg(IPointer(fVar.Dta));
          end;
{$IFDEF WINDOWS}
        btExtended, btDouble: {8 bytes — Extended == Double on Win64} begin
            StoreReg(double(fvar.dta^));
          end;
{$ELSE}
        btExtended: begin
            StoreStack(fVar.Dta^, fVar^.aType.RealSize);
          end;
        btDouble: {8 bytes} begin
            StoreReg(double(fvar.dta^));
          end;
{$ENDIF}
        btCurrency: {8 bytes} begin
            StoreReg(IPointer(fvar.dta^));
          end;
        btSingle: {4 bytes} begin
            StoreReg(single(fvar.dta^));
          end;

        btChar,
        btU8,
        btS8: begin
            StoreReg(IPointer(byte(fVar^.dta^)));
          end;
        btWideChar,
        btu16, btS16: begin
            StoreReg(IPointer(word(fVar^.dta^)));
          end;
        btu32, bts32: begin
            StoreReg(IPointer(cardinal(fVar^.dta^)));
          end;
        btPchar:
          begin
            if pointer(fvar^.dta^) = nil then
              StoreReg(IPointer(@EmptyPchar))
            else
              StoreReg(IPointer(fvar^.dta^));
          end;
        btclass, btinterface, btString:
          begin
            StoreReg(IPointer(fvar^.dta^));
          end;
        btWideString: begin
            StoreReg(IPointer(fvar^.dta^));
          end;
        btUnicodeString: begin
            StoreReg(IPointer(fvar^.dta^));
          end;

        btProcPtr:
          begin
            GetMem(p, PointerSize2);
            if Longint(FVar.Dta^) <> 0 then
              TMethod(p^) := MKMethod(Self, Longint(FVar.Dta^))
            else
            begin
              // Method-number 0 means the value carries a raw TMethod instead:
              // a host method, or a script method belonging to another TPSExec
              // (the event-property read path stores it in the second/third
              // pointer-sized slots). Passing it to the host would otherwise
              // silently become TMethod(nil, nil) — as if nil was assigned.
              TMethod(p^).Data := Pointer(Pointer((IPointer(FVar.Dta)+PointerSize))^);
              TMethod(p^).Code := Pointer(Pointer((IPointer(FVar.Dta)+PointerSize2))^);
            end;
            // 7bit
            {$IFDEF WINDOWS}
            StoreReg(IPointer(p));
            GetMem(pp, 2*PointerSize);
            pp^ := 1; {tag: raw memory block to free after the call}
            Pointer(Pointer(IPointer(pp)+PointerSize)^) := p;
            CallData.Add(pp);
            {$ELSE}
            // Если адреса обработчика не помещаются в регистры (если обработчик
            // идет пятым параметром), то сохраняем их в стеке. Причем регистр
            // R9 системой не будет использоваться, т. е. не надо делать
            // пятый параметр заглушкой. То, что в сохранится в стеке и будет
            // пятым параметром.
            // P.S. Пятый, не шестой, потому что первый параметр начинается с RSI,
            // а не RDI.
            // P.P.S. Обработчик занимает два регистра, но оба регистра идут, как
            // один параметр в процедуру.
            if RegUsage = 5 then StoreReg(IPointer(0));
            StoreReg(IPointer(TMethod(p^).Code));
            StoreReg(IPointer(TMethod(p^).Data));
            FreeMem(p);
            {$ENDIF}
            {StoreStack(p^, Pointersize2);
            FreeMem(p); }
            //
          end;

        bts64:
          begin
            StoreReg(IPointer(int64(fvar^.dta^)));
        end;
      end; {case}
    end;
    Result := True;
  end;
begin
  InnerfuseCall := False;
  if Address = nil then
    exit; // need address
  SetLength(Stack, 0);
  CallData := TPSList.Create;
  res := rp(res);
  if res <> nil then
    res.VarParam := true;
  try
{$IFNDEF WINDOWS}
    (*_RSI := 0;
    _RDI := 0;
    _XMM4 := 0;
    _XMM5 := 0;
    _XMM6 := 0;
    _XMM7 := 0;*)
    RegUsageFloat := 0;
{$ENDIF}
  _XMM0 := 0;
  FillChar(Registers, Sizeof(REgisters), 0);
    _RAX := 0;
    RegUsage := 0;

    // 7bit  Это исправление не работает в Linux64
    {$IFDEF FPC}
    // FIX FOR FPC constructor calls
    {if (Integer(CallingConv) and 128) <> 0 then begin
      IsVirtualCons := true;
    end else
      IsVirtualCons:= false;
    if IsVirtualCons then begin
      if not GetPtr(rp(Params[0])) then exit; // this goes first
      StoreReg(IPointer(_Self));
      Params.Delete(0);
    end else    }
    {$ENDIF}
    //

    if assigned(_Self) then begin
      StoreReg(IPointer(_Self));
    end;
    if assigned(res) and (res^.atype.basetype = btSingle) then begin
      Registers.Singlebits  := Registers.Singlebits or 256;
    end;
{$IFDEF PS_RESBEFOREPARAMETERS}
   if assigned(res) then begin
    case res^.aType.BaseType of
      {$IFDEF x64_string_result_as_varparameter}
      btstring, btWideString, btUnicodeString,
      {$ENDIF}
      btInterface, btArray, btVariant, btStaticArray:
        GetPtr(res);
      btRecord,
      btSet:
        begin
          {$IFDEF WINDOWS}
          if res.aType.RealSize > PointerSize then GetPtr(res);
          {$ELSE}
          // Linux x86-64 (SysV): FPC returns 9..16-byte records in RAX:RDX,
          // so no hidden result pointer must be pushed as the first register.
          if res.aType.RealSize > 16 then GetPtr(res);
          {$ENDIF}
        end;
    end;
   end;
{$ENDIF}
    for I := 0 to Params.Count - 1 do
    begin
      if not GetPtr(rp(Params[I])) then
        Exit;
    end;
    if assigned(res) then begin
{$IFNDEF PS_RESBEFOREPARAMETERS}
      case res^.aType.BaseType of
        {$IFDEF x64_string_result_as_varparameter}
        btstring, btWideString, btUnicodeString,
        {$ENDIF}
        btInterface, btArray, btVariant, btStaticArray:
          GetPtr(res);
        btRecord,
        btSet:
          begin
            {$IFDEF WINDOWS}
            if res.aType.RealSize > PointerSize then GetPtr(res);
            {$ELSE}
            // Linux: 9..16-byte record results come back in RAX:RDX.
            if res.aType.RealSize > 16 then GetPtr(res);
            {$ENDIF}
          end;
      end;
{$ENDIF}
      {$IFDEF WINDOWS}
      if (length(Stack) mod 16) <> 0 then begin
        SetLength(Stack, Length(Stack)+16 - (Length(Stack) mod 16));
      end;
      {$ENDIF} 
      if Stack = nil then pp := nil else pp := @Stack[Length(Stack)-8];
{$IFDEF WINDOWS}
      Registers.Stack := pp;
      Registers.Items := Length(Stack) div 8;
      x64call(Address, _RAX, _XMM0, Registers);
{$ELSE}
      x64call(Address, _RAX, Registers, pp, Length(Stack) div 8, _XMM0, _RDX);
{$ENDIF}
      case res^.aType.BaseType of
        btRecord, btSet:
          begin
            case res.aType.RealSize of
              1: byte(res.Dta^) := _RAX;
              2: word(res.Dta^) := _RAX;
              3,
              4: Longint(res.Dta^) := _RAX;
              5,6,7,8: IPointer(res.dta^) := _RAX;
              9..16: begin
                // x86-64 SysV: records of 9..16 bytes are returned in RAX:RDX
                PInt64(res.dta)^ := Int64(_RAX);
                PInt64(PAnsiChar(res.dta) + 8)^ := Int64(_RDX);
              end;
            end;
          end;
        btSingle:      tbtsingle(res.Dta^) := _XMM0;
        btDouble:      tbtdouble(res.Dta^) := _XMM0;
{$IFNDEF WINDOWS}
        btExtended:    // SysV x86-64 (FPC) returns 80-bit Extended in ST0 (x87), not XMM0.
          begin
            asm
              fstp tbyte ptr [tmp]
            end;
            tbtextended(res.Dta^) := tmp;
          end;
{$ELSE}
        btExtended:    tbtextended(res.Dta^) := _XMM0;      // Win64: Extended == Double (8 bytes)
{$ENDIF}
        btchar,btU8, btS8:    tbtu8(res.dta^) := _RAX;
        btWideChar, btu16, bts16:  tbtu16(res.dta^) := _RAX;
        btClass : IPointer(res.dta^) := _RAX;
        btu32,bts32:   tbtu32(res.dta^) := _RAX;
        btPChar:       pansichar(res.dta^) := Pansichar(_RAX);
        bts64: tbts64(res.dta^) := Int64(_RAX);
        btCurrency:    tbts64(res.Dta^) := Int64(_RAX);
        btInterface,
        btVariant,
        {$IFDEF x64_string_result_as_varparameter}
        btWidestring,btUnicodestring, btstring ,
        {$ENDIF}
        btStaticArray, btArray:;
        {$IFNDEF x64_string_result_as_varparameter}
        btUnicodeString, btWideString, btstring:  Int64(res.dta^) := _RAX;
        {$ENDIF}
      else
        exit;
      end;
    end else begin
      {$IFDEF WINDOWS}
      if (length(Stack) mod 16) <> 0 then begin
        SetLength(Stack, Length(Stack)+16 - (Length(Stack) mod 16));
      end;
      {$ENDIF} 
	if Stack = nil then pp := nil else  pp := @Stack[Length(Stack) -8];
{$IFDEF WINDOWS}
Registers.Stack := pp;
        Registers.Items := Length(Stack) div 8;
        x64call(Address, _RAX, _XMM0, Registers);
{$ELSE}
        x64call(Address, _RAX, Registers, pp, Length(Stack) div 8, _XMM0, _RDX);
{$ENDIF}
    end;
    Result := True;
  finally
    //tmp := CallData.Count;
    for i := CallData.Count -1 downto 0 do
    begin
      pp := CallData[i];
      case pp^ of
        0: DestroyOpenArray(Self, Pointer(pp));
        1: FreeMem(Pointer(Pointer(IPointer(pp)+PointerSize)^));
      end;
    end;
    CallData.Free;
  end;
end;


