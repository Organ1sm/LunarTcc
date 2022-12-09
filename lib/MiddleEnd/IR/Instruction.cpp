//
// Created by Organ1sm.
//

#include <iostream>
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"
#include "fmt/color.h"
#include "fmt/core.h"

std::string &JumpInstruction::GetTargetLabelName() { return Target->GetName(); }

std::string &BranchInstruction::GetTrueLabelName() { return TrueTarget->GetName(); }

std::string &BranchInstruction::GetFalseLabelName() { return FalseTarget->GetName(); }

void LoadInstruction::ConstructorHelper()
{
    auto PtrLevel = this->GetTypeRef().GetPointerLevel();

    // Globals are handled differently, it is implicitly assumed that they
    // have 1 pointer level more, even though their IRType does not reflect this
    if (PtrLevel != 0 && !Source->IsGlobalVar())
        PtrLevel--;

    this->GetTypeRef().SetPointerLevel(PtrLevel);
}
//=--------------------------------------------------------------------------=//
//=------------------------- Print functions --------------------------=//
//=--------------------------------------------------------------------------=//
std::string Instruction::AsString(Instruction::InstructionKind IK)
{
    switch (IK)
    {
        case And: return "and";
        case Or: return "or";
        case XOr: return "xor";
        case LSL: return "lsl";
        case LSR: return "lsr";
        case Add: return "add";
        case AddF: return "addf";
        case Sub: return "sub";
        case SubF: return "subf";
        case Mul: return "mul";
        case MulF: return "mulf";
        case Div: return "div";
        case DivU: return "divu";
        case DivF: return "divf";
        case Mod: return "mod";
        case ModU: return "modu";
        case SExt: return "sext";
        case ZExt: return "zext";
        case Trunc: return "trunc";
        case FloatToInt: return "ftoi";
        case IntToFloat: return "itof";
        case BitCast: return "bitcast";
        case Call: return "call";
        case Jump: return "j";
        case Branch: return "br";
        case Ret: return "ret";
        case Load: return "load";
        case Store: return "store";
        case MemCopy: return "memcopy";
        case StackAlloc: return "salloc";
        case GetELemPtr: return "gep";
        case Cmp: return "cmp";
        case CmpF: return "cmpf";

        default: assert(!"Unknown instruction kind."); break;
    }
}

void Instruction::PrintInst(bool ShowColor, const std::string Context) const
{
    if (ShowColor)
    {
        fmt::print(fmt::emphasis::bold | fg(fmt::color::green),
                   InstFormat,
                   "",
                   Context.empty() ? AsString(InstKind) :
                                     AsString(InstKind) + "." + Context);
    }
    else
    {
        fmt::print("\t{}\t",
                   Context.empty() ? AsString(InstKind) :
                                     AsString(InstKind) + "." + Context);
    }
}

void BinaryInstruction::Print(bool ShowColor) const
{
    std::string Format = "{Dest}, {L}, {R}\n";

    PrintInst(ShowColor);
    fmt::print(Format,
               fmt::arg("Dest", ValueString()),
               fmt::arg("L", LHS->ValueString()),
               fmt::arg("R", RHS->ValueString()));
}

void UnaryInstruction::Print(bool ShowColor) const
{
    std::string Format = "{Dest}, {Op}\n";

    PrintInst(ShowColor);
    fmt::print(Format,
               fmt::arg("Dest", ValueString()),
               fmt::arg("Op", Op->ValueString()));
}

const char *CompareInstruction::GetRelationString() const
{
    switch (Relation)
    {
        case EQ: return "eq";
        case NE: return "ne";
        case LT: return "lt";
        case GT: return "gt";
        case LE: return "le";
        case GE: return "ge";
        default: assert(!"Unhandled comparison relation.");
    }
}

void CompareInstruction::InvertRelation()
{
    switch (Relation)
    {
        case EQ: Relation = NE; break;
        case NE: Relation = EQ; break;
        case LT: Relation = GE; break;
        case GT: Relation = LE; break;
        case LE: Relation = GT; break;
        case GE: Relation = LT; break;
        default: break;
    }
}

void CompareInstruction::Print(bool ShowColor) const
{
    std::string Format = "{Value}, {L}, {R}\n";

    PrintInst(ShowColor, GetRelationString());
    fmt::print(Format,
               fmt::arg("Value", ValueString()),
               fmt::arg("L", LHS->ValueString()),
               fmt::arg("R", RHS->ValueString()));
}

void CallInstruction::Print(bool ShowColor) const
{
    std::string Format = "{RetValue}{FuncName}({Args})\n";
    std::string RetValue {};
    std::string ArgsStr {};

    if (!ValueType.IsVoid())
        RetValue = ValueString() + ", ";

    int i = 0;
    for (auto Arg : Arguments)
    {
        if (i > 0)
            ArgsStr += ", ";

        ArgsStr += Arg->ValueString();
        i++;
    }

    PrintInst(ShowColor);
    fmt::print(Format,
               fmt::arg("RetValue", RetValue),
               fmt::arg("FuncName", Name),
               fmt::arg("Args", ArgsStr));
}

void JumpInstruction::Print(bool ShowColor) const
{
    std::string Format = "<{Label}>\n";

    PrintInst(ShowColor);
    fmt::print(Format, fmt::arg("Label", Target->GetName()));
}

void BranchInstruction::Print(bool ShowColor) const
{
    std::string Format = "{Condition}, <{TrueTarget}>{FalseTarget}\n";
    std::string FTStr {};

    if (FalseTarget)
        FTStr += ", <" + FalseTarget->GetName() + ">";

    PrintInst(ShowColor);
    fmt::print(Format,
               fmt::arg("Condition", Condition->ValueString()),
               fmt::arg("TrueTarget", TrueTarget->GetName()),
               fmt::arg("FalseTarget", FTStr));
}

void ReturnInstruction::Print(bool ShowColor) const
{
    std::string Format = "{Value}\n";

    PrintInst(ShowColor);
    if (ReturnVal)
        fmt::print(Format, fmt::arg("Value", ReturnVal->ValueString()));
}

void StackAllocationInstruction::Print(bool ShowColor) const
{
    std::string Format = "{Dest}\n";

    PrintInst(ShowColor);
    fmt::print(Format, fmt::arg("Dest", ValueString()));
}

void GetElemPointerInstruction::Print(bool ShowColor) const
{
    auto Format = "{Dest}, {Source}, {Index}\n";

    PrintInst(ShowColor);
    fmt::print(Format,
               fmt::arg("Dest", ValueString()),
               fmt::arg("Source", Source->ValueString()),
               fmt::arg("Index", Index->ValueString()));
}

void StoreInstruction::Print(bool ShowColor) const
{
    std::string Format = "[{Dest}], {Source}\n";

    PrintInst(ShowColor);
    fmt::print(Format,
               fmt::arg("Dest", Destination->ValueString()),
               fmt::arg("Source", Source->ValueString()));
}

void LoadInstruction::Print(bool ShowColor) const
{
    std::string Format = "{Dest}, [{Source}{Offset}]\n";
    std::string OffsetStr {};

    if (Offset)
        OffsetStr += " + " + Offset->ValueString();

    PrintInst(ShowColor);
    fmt::print(Format,
               fmt::arg("Dest", ValueString()),
               fmt::arg("Source", Source->ValueString()),
               fmt::arg("Offset", OffsetStr));
}

void MemoryCopyInstruction::Print(bool ShowColor) const
{
    std::string Format = "{Dest}, {Source}, {Number}\n";

    PrintInst(ShowColor);
    fmt::print(Format,
               fmt::arg("Dest", Dest->ValueString()),
               fmt::arg("Source", Source->ValueString()),
               fmt::arg("Number", N));
}
