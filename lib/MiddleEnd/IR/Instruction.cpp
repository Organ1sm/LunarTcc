//
// Created by Organ1sm.
//

#include <iostream>
#include "MiddleEnd/IR/Instruction.hpp"
#include "MiddleEnd/IR/BasicBlock.hpp"

std::string &JumpInstruction::GetTargetLabelName() { return Target->GetName(); }

std::string &BranchInstruction::GetTrueLabelName() { return TrueTarget->GetName(); }

std::string &BranchInstruction::GetFalseLabelName() { return FalseTarget->GetName(); }

void LoadInstruction::ConstructorHelper()
{
    auto PtrLevel = this->GetTypeRef().GetPointerLevel();

    if (PtrLevel != 0)
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
        case And:
            return "and";
        case Or:
            return "or";
        case Add:
            return "add";
        case Sub:
            return "sub";
        case Mul:
            return "mul";
        case Div:
            return "div";
        case Mod:
            return "mod";
        case SExt:
            return "sext";
        case ZExt:
            return "zext";
        case Trunc:
            return "trunc";
        case FloatToInt:
            return "ftoi";
        case IntToFloat:
            return "itof";
        case Call:
            return "call";
        case Jump:
            return "j";
        case Branch:
            return "br";
        case Ret:
            return "ret";
        case Load:
            return "load";
        case Store:
            return "store";
        case MemCopy:
            return "memcopy";
        case StackAlloc:
            return "salloc";
        case GetELemPtr:
            return "gep";
        case Cmp:
            return "cmp";
        case Mov:
            return "mov";
        default:
            assert(!"Unknown instruction kind.");
            break;
    }
}

void BinaryInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "\t";
    std::cout << ValueString() << ", ";
    std::cout << LHS->ValueString() << ", ";
    std::cout << RHS->ValueString() << std::endl;
}

void UnaryInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "\t";
    std::cout << ValueString() << ", ";
    std::cout << Op->ValueString() << std::endl;
}

const char *CompareInstruction::GetRelationString() const
{
    switch (Relation)
    {
        case EQ:
            return "eq";
        case NE:
            return "ne";
        case LT:
            return "lt";
        case GT:
            return "gt";
        case LE:
            return "le";
        case GE:
            return "ge";
        default:
            assert(!"Unhandled comparison relation.");
    }
}

void CompareInstruction::InvertRelation()
{
    switch (Relation)
    {
        case EQ:
            Relation = NE;
            break;
        case NE:
            Relation = EQ;
            break;
        case LT:
            Relation = GE;
            break;
        case GT:
            Relation = LE;
            break;
        case LE:
            Relation = GT;
            break;
        case GE:
            Relation = LT;
            break;
        default:
            break;
    }
}

void CompareInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "." << GetRelationString() << "\t";
    std::cout << ValueString() << ", ";
    std::cout << LHS->ValueString() << ", ";
    std::cout << RHS->ValueString() << std::endl;
}

void CallInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "\t";

    if (!ValueType.IsVoid())
        std::cout << ValueString() << ", ";

    std::cout << Name << "(";

    int i = 0;
    for (auto Arg : Arguments)
    {
        if (i > 0)
            std::cout << ", ";

        std::cout << Arg->ValueString();
        i++;
    }

    std::cout << ")" << std::endl;
}

void JumpInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "\t";
    std::cout << "<" << Target->GetName() << ">" << std::endl;
}

void BranchInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "\t";
    std::cout << Condition->ValueString() << ", ";
    std::cout << "<" << TrueTarget->GetName() << ">";

    if (FalseTarget)
        std::cout << ", <" << FalseTarget->GetName() << ">";

    std::cout << std::endl;
}

void ReturnInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "\t";
    std::cout << ReturnVal->ValueString() << std::endl;
}

void StackAllocationInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "\t";
    std::cout << ValueString() << std::endl;
}

void GetElemPointerInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "\t";
    std::cout << ValueString() << ", ";
    std::cout << Source->ValueString();

    std::string Str = ", ";
    Str += Index->ValueString();

    std::cout << Str << std::endl;
}

void StoreInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "\t";
    std::cout << "[" << Destination->ValueString() << "], ";
    std::cout << Source->ValueString() << std::endl;
}

void LoadInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << '\t';
    std::cout << ValueString() << ", ";
    std::cout << "[" << Source->ValueString();
    if (Offset)
        std::cout << " + " << Offset->ValueString();
    std::cout << "]" << std::endl;
}

void MemoryCopyInstruction::Print() const
{
    std::cout << "\t" << AsString(InstKind) << "\t";
    std::cout << Dest->ValueString() << ", ";
    std::cout << Src->ValueString() << ", ";
    std::cout << N << std::endl;
}
