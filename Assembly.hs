-- AT&T x86 Assembly
module Assembly where
import Struct
import DeBruijn
import Data.List (intercalate, replicate)

-- 64-Bit General Purpose Registers (GPRs)
data Reg64 =
    RAX | RBX | RCX | RDX
  | RBP | RSI | RDI | RSP
  | R8  | R9  | R10 | R11
  | R12 | R13 | R14 | R15

data Mem =
    MemReg Reg -- (%rax); load data from memory address
  | MemVar String -- var(,1); load the address of a var
  | MemOff Int Reg -- -4(%rax) = %rax + (-4); load data from memory address + offset
  | MemAdd Reg Reg -- (%rax,%rbx) = (%rax + %rbx); load data from fst mem addr + snd mem addr
  | MemMul Reg Int -- (%rax,2) = (%rax * 2); load data from mem addr * multiplier
  | MemAnM Reg Reg Int -- (%rax,%rbx,4) = (%rax+4*%rbx); load data from fst mem addr + multiplier * snd mem addr
  
-- Operand
data Op =
    Reg Reg64
  | Mem Mem
  | Con Int

-- Condition
data Cond = JE | JNE | JZ | JG | JGE | JL | JLE

-- Instruction
data Instr =
    MOV Op Op
  | PUSH Op
  | POP Op
  | LEA Op
  | ADD Op Op
  | SUB Op Op
  | INC Op
  | DEC Op
  | IMUL Op Op
  | IDIV Op Op
  | AND Op Op
  | OR Op Op
  | XOR Op Op
  | NOT Op
  | Neg Op
  | SHL Op
  | SHR Op
  | JMP String
  | JCOND Cond String
  | CMP Op Op
  | CALL String
  | RET

-- Directive
data Drctv =
    DirData
  | DirGlobl [String]
  | DirLocal [String]
  | DirQuad [Int]
  | DirString String
  | DirLabel String

data Command =
    CmdDir Drctv
  | CmdIns Instr

-- Checks if instruction syntax is valid
checkInstr :: Instr -> Bool
checkInstr (MOV (Reg _) (Reg _)) = True
checkInstr (MOV (Reg _) (Mem _)) = True
checkInstr (MOV (Mem _) (Reg _)) = True
checkInstr (MOV (Con _) (Reg _)) = True
checkInstr (MOV (Con _) (Mem _)) = True
checkInstr (PUSH (Reg _)) = True
checkInstr (PUSH (Mem _)) = True
checkInstr (PUSH (Con _)) = True
checkInstr (POP (Reg _)) = True
checkInstr (POP (Mem _)) = True
checkInstr (LEA (Mem _) (Reg _)) = True
checkInstr (ADD (Reg _) (Reg _)) = True
checkInstr (ADD (Mem _) (Reg _)) = True
checkInstr (ADD (Reg _) (Mem _)) = True
checkInstr (ADD (Con _) (Reg _)) = True
checkInstr (ADD (Con _) (Mem _)) = True
checkInstr (SUB (Reg _) (Reg _)) = True
checkInstr (SUB (Mem _) (Reg _)) = True
checkInstr (SUB (Reg _) (Mem _)) = True
checkInstr (SUB (Con _) (Reg _)) = True
checkInstr (SUB (Con _) (Mem _)) = True
checkInstr (INC (Reg _)) = True
checkInstr (INC (Mem _)) = True
checkInstr (DEC (Reg _)) = True
checkInstr (DEC (Mem _)) = True
checkInstr (IMUL (Reg _) (Reg _)) = True
checkInstr (IMUL (Mem _) (Reg _)) = True
checkInstr (IMUL (Con _) (Reg _) (Reg _)) = True
checkInstr (IMUL (Con _) (Mem _) (Reg _)) = True
checkInstr (IDIV (Reg _)) = True
checkInstr (IDIV (Mem _)) = True
checkInstr (AND (Reg _) (Reg _)) = True
checkInstr (AND (Mem _) (Reg _)) = True
checkInstr (AND (Reg _) (Mem _)) = True
checkInstr (AND (Con _) (Reg _)) = True
checkInstr (AND (Con _) (Mem _)) = True
checkInstr (OR (Reg _) (Reg _)) = True
checkInstr (OR (Mem _) (Reg _)) = True
checkInstr (OR (Reg _) (Mem _)) = True
checkInstr (OR (Con _) (Reg _)) = True
checkInstr (OR (Con _) (Mem _)) = True
checkInstr (XOR (Reg _) (Reg _)) = True
checkInstr (XOR (Mem _) (Reg _)) = True
checkInstr (XOR (Reg _) (Mem _)) = True
checkInstr (XOR (Con _) (Reg _)) = True
checkInstr (XOR (Con _) (Mem _)) = True
checkInstr (NOT (Reg _)) = True
checkInstr (NOT (Mem _)) = True
checkInstr (NEG (Reg _)) = True
checkInstr (NEG (Mem _)) = True
checkInstr (SHL (Con _) (Reg _)) = True -- Note: constant at most 8-bit
checkInstr (SHL (Con _) (Mem _)) = True -- Note: constant at most 8-bit
--checkInstr (SHL %cl (Reg _)) = True
--checkInstr (SHL %cl (Mem _)) = True
checkInstr (SHR (Con _) (Reg _)) = True -- Note: constant at most 8-bit
checkInstr (SHR (Con _) (Mem _)) = True -- Note: constant at most 8-bit
--checkInstr (SHR %cl (Reg _)) = True
--checkInstr (SHR %cl (Mem _)) = True
checkInstr (JMP _) = True
checkInstr (JCOND _) = True
checkInstr (CMP (Reg _) (Reg _)) = True
checkInstr (CMP (Mem _) (Reg _)) = True
checkInstr (CMP (Reg _) (Mem _)) = True
checkInstr (CMP (Con _) (Reg _)) = True
checkInstr (CALL _) = True
checkInstr (RET) = True
checkInstr _ = False


------------------------------ Show instances ------------------------------

instance Show Reg64 where
  show RAX = "%rax"
  show RBX = "%rbx"
  show RCX = "%rcx"
  show RDX = "%rdx"
  show RBP = "%rbp"
  show RSI = "%rsi"
  show RDI = "%rdi"
  show RSP = "%rsp"
  show R8  = "%r8 "
  show R9  = "%r9 "
  show R10 = "%r10"
  show R11 = "%r11"
  show R12 = "%r12"
  show R13 = "%r13"
  show R14 = "%r14"
  show R15 = "%r15"

instance Show Mem where
  show (MemReg r) = parens (show r)
  show (MemVar v) = v ++ "(,1)"
  show (MemOff i r) = show i ++ parens (show r)
  show (MemAdd r1 r2) = parens (show r1 ++ "," ++ show r2)
  show (MemMul r i) = parens (show r ++ "," ++ show i)
  show (MemAnM r1 r2 i) = parens (show r1 ++ "," ++ show r2 ++ "," ++ show i)

instance Show Op where
  show (Reg r) = show r
  show (Mem m) = show m
  show (Con i) = '$' : show i

instance Show Cond where
  show JE = "JE"
  show JNE = "JNE"
  show JZ = "JZ"
  show JG = "JG"
  show JGE = "JGE"
  show JL = "JL"
  show JLE = "JLE"

instance Show Instr where
  show (MOV o1 o2) = "MOVQ " ++ show o1 ++ "," ++ show o2
  show (PUSH o) = "PUSHQ " ++ show o
  show (POP o) = "POPQ " ++ show o
  show (LEA o) = "LEAQ " ++ show o
  show (ADD o1 o2) = "ADDQ " ++ show o1 ++ "," ++ show o2
  show (SUB o1 o2) = "SUBQ " ++ show o1 ++ "," ++ show o2
  show (INC o) = "INCQ " ++ show o
  show (DEC o) = "DECQ " ++ show o
  show (IMUL o1 o2) = "IMULQ " ++ show o1 ++ "," ++ show o2
  show (IDIV o1 o2) = "IDIVQ " ++ show o1 ++ "," ++ show o2
  show (AND o1 o2) = "ANDQ " ++ show o1 ++ "," ++ show o2
  show (OR o1 o2) = "ORQ " ++ show o1 ++ "," ++ show o2
  show (XOR o1 o2) = "XORQ " ++ show o1 ++ "," ++ show o2
  show (NOT o) = "NOTQ " ++ show o
  show (NEG o) = "NEGQ " ++ show o
  show (SHL o) = "SHLQ " ++ show o
  show (SHR o) = "SHRQ " ++ show o
  show (JMP s) = "JMP " ++ s
  show (JCOND c s) = show c ++ " " ++ s
  show (CMP o1 o2) = "CMPQ " ++ show o1 ++ "," ++ show o2
  show (CALL s) = "CALL " ++ s
  show (RET) = "RET"

instance Show Drctv where
  show (DirData) = ".data"
  show (DirGlobl s) = ".globl " ++ intercalate ", " s
  show (DirLocal s) = ".local " ++ intercalate ", " s
  show (DirQuad i) = ".quad " ++ intercalate ", " [show j | j <- i]
  show (DirString s) = ".string " ++ show s -- escapes and quotes
  show (DirLabel s) = s ++ ":"

instance Show Command where
  show (CmdDir d) = show d
  show (CmdIns i) = replicate 4 ' ' ++ show i

----------------------------------------------------------------------------

naiveCompile :: DBTm -> [Instr]
naiveCompile program = error "TODO"
  where
    {-
      MOVQ $0, %rλ
      dbHNF (DBVar i) =
        INCQ i
        SUBQ i, %rλ
        MOVQ %rλ, %rdi # set exit code
      dbHNF (DBApp t u) =
        case dbCBN t of
          DBLam t' -> dbHNF (dbSubst t' 0 u)
          t' -> DBApp t' u
      dbHNF (DBLam t) =
        inc %rλ
        dbHNF t

     -}

    -- Put initial term (program) onto the stack
    compileAST :: DBTm -> ()
    compileAST t = error "TODO"

    --Set exit code = # of λs - head index - 1
    -- e.g. λλ1 => 0 (Church true) and λλ0 => 1 (false)
    compileExit :: ()
    compileExit = error "TODO"
    
    -- Machine code that performs the subtitution t[m->u] for registers t,m,u
    -- args: %rdi, %rsi, %rdx
    substCode :: Reg64 -> Reg64 -> Reg64 -> [Instr]
    substCode = [ -- out-of-place?
      (),
      ()
    ]

    lincFVs = "incFVs"
    lincFVsH = "incFVsH"
    lincFVsEnd = "incFVsEnd"
    lincFVsVar = "incFVsVar"
    lincFVsApp = "incFVsApp"
    lincFVsLam = "incFVsLam"
    rs = R8 -- number of ASTs on the stack to operate on
    rk = R9
    r15 = R15

    -- Machine code that increments all free vars >=k in n
    -- CALLable, saves and restores all registers
    -- Top of stack should be pointer to AST (which gets popped)
    incFVsCode :: [Command]
    incFVsCode = -- in-place
      [
        CmdDir (DirLabel lincFVs),
        CmdIns (PUSH (Reg rs)),
        CmdIns (PUSH (Reg rk)),
        CmdIns (PUSH (Reg r15)),
        CmdIns (POP (Reg r15)),
        CmdIns (PUSH (Con 0)),
        CmdIns (PUSH (Reg r15)),
        CmdIns (MOV (Con 1) (Reg rs)),
        CmdIns (DirLabel lincFVsH),
        CmdIns (CMP (Con 0) (Reg rs)),
        CmdIns (JCOND JE lincFVsEnd),
        CmdIns (POP (Reg r15)),
        CmdIns (POP (Reg rk)),
        CmdIns (CMPQ (Con 1) (Mem (MemReg r15))),
        CmdIns (JCOND JL lincFVsVar), -- if <, var
        CmdIns (JCOND JE lincFVsApp), -- if =, app
        CmdDir (DirLabel lincFVsLam), -- else, lam
        -- lam code
        CmdIns (INC (Reg rk)),
        CmdIns (PUSH (Reg rk)),
        CmdIns (PUSH (Mem (MemOff 1 r15))),
        CmdIns (JMP lincFVsH),
        
        CmdDir (DirLabel lincFVsVar),
        -- var code
        CmdIns (DEC (Reg rs)),
        CmdIns (CMP (Reg rk) (Mem (MemOff 1 r15))),
        CmdIns (JL lincFVsH),
        CmdIns (INC (Mem (MemOff 1 r15))),
        CmdIns (JMP lincFVsH),
        
        CmdDir (DirLabel lincFVsApp),
        -- app code
        CmdIns (INC (Reg rs)),
        CmdIns (PUSH (Reg rk)),
        CmdIns (PUSH (Mem (MemOff 1 r15))),
        CmdIns (PUSH (Reg rk)),
        CmdIns (PUSH (Mem (MemOff 2 r15))),
        CmdIns (JMP lincFVsH),
        
        CmdDir (DirLabel lincFVsEnd),
        CmdIns (POP (Reg r15)),
        CmdIns (POP (Reg rk)),
        CmdIns (POP (Reg rs)),
        CmdIns RET
      ]
